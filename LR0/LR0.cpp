#include "stdafx.h"
#include <unordered_set>
#include <map>
#include <string>
#include <fstream>
#include <iostream>

typedef std::unordered_set<char> set;
typedef std::string string;
typedef std::multimap<char, string> productions;

class grammar {
	set N; //nonterminals
	set T; //terminals
	productions P;
	char S; //start character. must be an element of nonterminals
	productions Items; //set of "items" (LR(0) items)

	//supplemental to the FIRST, FOLLOW, CLOSURE, and GOTO functions; so that recursion does not become exponential in those respective functions
	std::map<char, set> FIRSTS;
	std::map<char, set> FOLLOWS;
	std::map<productions, productions> CLOSURES;
	std::map<std::pair<productions, char>, productions> GOTOS;
public:
	grammar(productions, char);
	set& FIRST(char);
	set& FOLLOW(char);
	productions& CLOSURE(productions);
	productions& GOTO(productions, char);
	void print(std::ostream&);
};

grammar::grammar(productions p, char s) : N(), T(), P(p), S(s), Items() {
	for (productions::iterator i = P.begin(); i != P.end(); ++i) {
		N.insert(i->first);
		for (string::iterator j = i->second.begin(); j != i->second.end(); ++j) T.insert(*j);
	}
	for (set::iterator i = N.begin(); i != N.end(); ++i) if (T.count(*i)) T.erase(*i);
	for (set::iterator i = N.begin(); i != N.end(); ++i) FOLLOWS[*i] = set();
	for (set::iterator i = T.begin(); i != T.end(); ++i) FOLLOWS[*i] = set();

	//FOLLOW rule 1
	FOLLOWS[s].insert('$'); //initialize start symbol set

	//FOLLOW rule 2
	for (productions::iterator prod = P.begin(); prod != P.end(); ++prod) //for each production,
		if (prod->second.length() > 1) //(ignore productions resulting in a single character)
			for (string::iterator sym = ----(prod->second.end()); sym >= prod->second.begin(); --sym) { //for each N -> aXb,
				for (set::iterator ins = FIRST(*(sym + 1)).begin(); ins != FIRST(*(sym + 1)).end(); ++ins) //everything in FIRST(b) except e
					if (*ins != 'e') FOLLOWS[*sym].insert(*ins); //should be put into FOLLOWS[X]
				if (sym == prod->second.begin()) break;
			}

	//FOLLOW rule 3
	for (std::map<char, set> OLD; OLD != FOLLOWS; ) { //while we still have changes to make,
		OLD = FOLLOWS; //(save the changes made last time)
		for (productions::iterator prod = P.begin(); prod != P.end(); ++prod) //for each production
			for (string::iterator sym = --(prod->second.end()); sym >= prod->second.begin(); --sym) { //go backward thru each production; for each N -> aXb,
				for (set::iterator ins = FOLLOWS[prod->first].begin(); ins != FOLLOWS[prod->first].end(); ++ins) //everything in FOLLOWS[N]
					FOLLOWS[*sym].insert(*ins); //should be put into FOLLOWS[X]
				if (sym == prod->second.begin() || !FIRST(*sym).count('e')) break; //if FIRST(X) contains e, continue (else break)
			}
	}

	//construction of I, the set of LR(0) items
	for (productions::iterator prod = P.begin(); prod != P.end(); ++prod)
		if (prod->second == "e")
			Items.insert(std::make_pair(prod->first, string(".")));
		else for (unsigned i = 0; i <= prod->second.length(); ++i)
			Items.insert(std::make_pair(prod->first, string(prod->second).insert(i, ".")));

	//pre-calculate CLOSURES
	for (productions::iterator item = Items.begin(); item != Items.end(); ++item)
		CLOSURE(productions({ *item }));

	//pre-calculate GOTOS
	for (productions::iterator item = Items.begin(); item != Items.end(); ++item)
		if (item->second.find('.') < item->second.length() - 1)
			GOTO(productions({ *item }), item->second[item->second.find('.') + 1]);
}
set& grammar::FIRST(char x) {
	if (FIRSTS.count(x)) return FIRSTS[x];
	if (x == 'e') return FIRSTS['e'] = set({ 'e' });
	if (T.count(x)) return FIRSTS[x] = set({ x });
	if (!N.count(x)) return FIRSTS[x] = set();
	FIRSTS[x] = set();
	for (productions::iterator prod = P.find(x); prod != P.end(); ++prod) {
		if (prod->first != x) break;
		for (string::iterator y = prod->second.begin(); y != prod->second.end(); ++y) {
			for (set::iterator ins = FIRST(*y).begin(); ins != FIRST(*y).end(); ++ins)
				FIRSTS[x].insert(*ins);
			if (!(FIRST(*y).count('e'))) break;
		}
	}
	return FIRSTS[x];
}
set& grammar::FOLLOW(char x) {
	if (FOLLOWS.count(x)) return FOLLOWS[x];
	return FOLLOWS[x] = set();
}
productions& grammar::CLOSURE(productions I) {
	if (CLOSURES.count(I)) return CLOSURES[I];
	CLOSURES[I] = I;
	productions J; //use this to tell if we still have added more to CLOSURES[I]
	while (J != CLOSURES[I]) { //if they're still equal after going thru all of the steps below, break
		J = CLOSURES[I]; //set equal to each other
		for (productions::iterator item = CLOSURES[I].begin(); item != CLOSURES[I].end(); ++item) //go thru all items in I
			if (item->second.find('.') < item->second.length() - 1) //ignore strings with the dot at the end of the string
				if (N.count(item->second[item->second.find('.') + 1])) //ignore any a.Tb , we only care about a.Nb
					for (productions::iterator prod = P.find(item->second[item->second.find('.') + 1]); prod != P.end(); ++prod) { //go thru all productions, starting with the first N that matches ours
						if (prod->first != item->second[item->second.find('.') + 1]) break; //stop once we don't have our N as left end of this production
						bool exists = false; //we don't want to add a pair that already exists
						for (productions::iterator j = CLOSURES[I].find(prod->first); j != CLOSURES[I].end(); ++j) //go thru all items in J
							if (j->first != prod->first) break; //stop when we're not looking at our N anymore
							else if (j->second == string(prod->second).insert(0, ".")) exists = true; //if it exists, mark as such
						if (!exists) CLOSURES[I].insert(std::make_pair(prod->first, string(prod->second).insert(0, "."))); //if it does not already exist, insert into I
					}
	}
	return CLOSURES[I];
}
productions& grammar::GOTO(productions I, char X) {
	if (GOTOS.count(std::make_pair(I, X))) return GOTOS[std::make_pair(I, X)];
	productions J;
	for (productions::iterator item = I.begin(); item != I.end(); ++item)
		if (item->second.find('.') < item->second.length() - 1)
			if (item->second[item->second.find('.') + 1] == X)
				for (productions::iterator items = Items.begin(); items != Items.end(); ++items)
					if (items->second.find('.') > 0 && items->first == item->first && items->second.length() == item->second.length())
						if (string(items->second).erase(items->second.find('.'), 1).insert(item->second.find('.'), ".") == item->second) {
							bool exists = false;
							for (productions::iterator j = J.find(items->first); j != J.end(); ++j) {
								if (j->first != items->first) break;
								if (j->second == items->second) exists = true;
							}
							if (!exists) J.insert(*items);
						}
	return GOTOS[std::make_pair(I, X)] = CLOSURE(J);
}
void grammar::print(std::ostream& output = std::cout) {
	//print productions
	for (productions::iterator i = P.begin(); i != P.end(); ++i)
		output << i->first << '-' << '>' << i->second << std::endl;
	set::iterator st;

	//print FIRSTS
	for (st = T.begin(); st != T.end(); ++st)
		output << "FIRST(" << *st << ") : { " << *FIRST(*st).begin() << " }\n";
	for (st = N.begin(); st != N.end(); ++st) {
		output << "FIRST(" << *st << ") : { ";
		for (set::iterator frst = FIRST(*st).begin(); ; ) {
			if (frst == FIRST(*st).end()) break;
			output << *frst;
			if (++frst == FIRST(*st).end()) break;
			output << ", ";
		}
		output << " }\n";
	}

	//print FOLLOWS
	for (st = T.begin(); st != T.end(); ++st) {
		output << "FOLLOW(" << *st << ") : { ";
		for (set::iterator flw = FOLLOW(*st).begin(); ; ) {
			if (flw == FOLLOW(*st).end()) break;
			output << *flw;
			if (++flw == FOLLOW(*st).end()) break;
			output << ", ";
		}
		output << " }\n";
	}
	for (st = N.begin(); st != N.end(); ++st) {
		output << "FOLLOW(" << *st << ") : { ";
		for (set::iterator flw = FOLLOW(*st).begin(); ; ) {
			if (flw == FOLLOW(*st).end()) break;
			output << *flw;
			if (++flw == FOLLOW(*st).end()) break;
			output << ", ";
		}
		output << " }\n";
	}

	//print CLOSURES
	for (productions::iterator item = Items.begin(); item != Items.end(); ++item) {
		output << "CLOSURE(" << item->first << "->" << item->second << "): { ";
		for (productions::iterator clo = CLOSURE(productions({ *item })).begin(); ; ) {
			output << clo->first << "->" << clo->second;
			if (++clo == CLOSURE(productions({ *item })).end()) break;
			output << ", ";
		}
		output << " }\n";
	}

	//print GOTOS
	for (productions::iterator item = Items.begin(); item != Items.end(); ++item)
		if (item->second.find('.') < item->second.length() - 1) {
			output << "GOTO(" << item->first << "->" << item->second << ", " << item->second[item->second.find('.') + 1] << "): { ";
			for (productions::iterator go = GOTO(productions({ *item }), item->second[item->second.find('.') + 1]).begin();
				go != GOTO(productions({ *item }), item->second[item->second.find('.') + 1]).end(); ) {
				output << go->first << "->" << go->second;
				if (++go == GOTO(productions({ *item }), item->second[item->second.find('.') + 1]).end()) break;
				output << ", ";
			}
			output << " }\n";
		}
}

int main() {
	std::ifstream source;
	source.open("grammar0.txt");
	productions p;
	char s;
	string input;
	std::getline(source, input);
	while (input != "$") std::getline(source, input);
	std::getline(source, input);
	s = input[0];
	while (input != "$") {
		p.insert(std::make_pair(input[0], input.substr(3)));
		std::getline(source, input);
	}
	grammar G(p, s);
	G.print();
	G.print(std::ofstream("SLR0.txt")); //so that I can open it in notepad without leaving the program running
	system("pause");
	return 0;
}

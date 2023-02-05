# FLP SIMPLIFY-BKG
prvni projekt do predmetu FLP v jazyce Haskell
Author: Vojtech Jahoda (xjahod06)
Úlohou v tomto projektu bylo naimplementovat algoritmus 4.3 z opory TIN, který popisuje jak lze zjednodušit Bezkontextova gramatika pomoci odstraněni nepotřebných symbolu.

## Pouziti
program lze spustit pomoci nasledujici syntaxe:
```
flp21-fun volby [vstup]
```
ve volbach lze nastavit pouze jeden ze 3 parametru
`-i` výpis sturuktury BKG z vnitřní reprezentace programu
`-1` výpis BKG po provedeni algoritmu 4,1
`-2` na stdout se vypíše BKG, která generuje stejný jazyk jako vstupní
gramatika, ale neobsahuje žádné zbytečné symboly. Pokud BKG
G¯ generuje neprázdný jazyk, je vypisovanou gramatikou BKG G′
z druhého kroku algoritmu 4.3. Pokud BKG G¯ generuje prázdný
jazyk, vypište výslednou minimalizovanou gramatiku přímo.

Vstup je očekáván ve formátu
```
seznam všech neterminálů
seznam všech terminálů
počáteční neterminál
pravidlo 1
.
.
.
pravidlo n
```
příklad:
```
A,B,S
a,b
S
A->AB
B->b
S->A
S->a
```

## Nedostatky řešení
Bohužel se mi nepovedlo implementovat zjednodušení bezkontextové gramatiky, ve které existuje neterminal, ktery zacykli BKG, ale přes více než jednu proměnnou
```
A,B,S,C
a,b
S
A->BC
C->A
B->b
S->A
S->a
```
v tomto případě je celý neterminal C nepotřebný, ale moje řešení si s tímto nezvládne poradit.
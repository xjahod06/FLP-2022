# SIMPLIFY-BKG
Vytvořte program, který odstraňuje zbytečné symboly z bezkontextové gramatiky. Postupujte podle algoritmu 4.3 z opory předmětu TIN, který pracuje ve dvou krocích.

## Rozhraní programu
Program bude možné spustit:

`flp21-fun volby [vstup]`

kde
1. vstup je jméno vstupního souboru (pokud není specifikováno, program čte standardní vstup) obsahujícího BKG.

2. volby jsou parametry ovlivňující chování programu:
- i vypíše se pouze načtená a do vaší vnitřní reprezentace převedená BKG na stdout. Nevypisujte jen načtený řetězec, tato volbaověřuje, že váš program dokáže gramatiku převést z vnitřní reprezentace.
- 1 vypíše se BKG G¯ (po prvním kroku algoritmu 4.3) na stdout.
- 2 na stdout se vypíše BKG, která generuje stejný jazyk jako vstupní gramatika, ale neobsahuje žádné zbytečné symboly. Pokud BKG G¯ generuje neprázdný jazyk, je vypisovanou gramatikou BKG G′z druhého kroku algoritmu 4.3. Pokud BKG G¯ generuje prázdný jazyk, vypište výslednou minimalizovanou gramatiku přímo.

## Formát vstupu
BKG G = (N, Σ, P, S) na vstupu odpovídá standardní definici BKG (viz oporu předmětu TIN). Pro zjednodušení je ale abeceda N podmnožinou množiny velkých písmen [A-Z], a abeceda Σ je podmnožinou množiny malých písmen [a-z]. Vstupní textová reprezentace BKG o n pravidlech má
formát:

```
    seznam všech neterminálů
    seznam všech terminálů
    počáteční neterminál
    pravidlo 1
    . . .
    pravidlo n
```

Seznam o m symbolech má tvar symbol 1,symbol 2,...,symbol m. Prázdné slovo je reprezentováno znakem #. Epsilon pravidlo má tedy tvar A->#. Například, reprezentace BKG z příkladu 4.1 opory předmětu TIN, s přidaným pravidlem S → ε, je následující:
```
S,A,B
a,b,c,d
S
S->#
S->AB
A->aAb
A->ab
B->cBd
B->cd
```

## Formát výstupu
Textová reprezentace BKG na výstupu má stejný formát jako BKG navstupu. V žádném případě ale neměňte označení stavů nebo dokonce sémantiku pravidel. Jen odstraňte zbytečné symboly a pravidla. Ponechanésymboly a pravidla musí zůstat stejná, jak byla zadána.
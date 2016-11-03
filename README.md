Scheme-tulkki joka toteuttaa jonkinlaisen osajoukon r5rs:stä.

## Puutteet

Oleellisimmat puutteet:
- call/cc
- quosiquote, makrot (ainoastaan hygieeniset ja leksikaalisella näkyvyysalueella
  varustetut define-macro -tyyliset makrot ovat tuettuja, samantyylisesti kuin
	gambitissa)
- vektorit
- numeeriset tyypit ovat suppeita
- merkkijonojen käsittelyfunktiot

Mutatoivissa primitiiveissä set! ja set-{car,cdr}! on rajoituksia. Käytännössä
nämä johtuvat siitä, että niiden kannalta oleellisissa tietorakenteissa käytetään
arvoja, joita ei voi muuttaa. (Arvot tyyppiä AST, mutta ympäristöissä IORef AST)
Suoraviivainen käyttö [esim: (define x '(1)) (set-cdr! x '(2 3))]
toimii. Erikoistapauksena myös (define y x) toimii aliaksena, olettaen että x on
viitearvo (esim. lista eikä kokonaisluku).

## Tuetut ominaisuudet

Tulkin rakenne on aika suoraviivainen, syötetty koodi menee reittiä
jäsentäjä (Parser.hs) -> syntaksianalyysi (Eval.hs) -> evaluointi (Eval.hs)

Tuetut erikoismuodot:
* define
* lambda
* if
* cond
* let
* let\*
* delay
* force
* define-macro

Primitiiveinä car ja cdr -johdoksista on toteutettuna c[ad]{1,5}r mutta lisäksi analysointivaiheessa
tunnistetaan mielivaltaisen pitkien johdosten käyttö (= c[ad]+r). Esim. kätevästi:

    >> (cadddddddddddddr '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
    14

Numerotyypeistä tuettuja ovat kokonaisluvut, liukuluvut ja rationaaliluvut. Jäsennin tosin ei suoraan tunnista rationaalilukuja vaan niitä voidaan muodostaa kokonaislukujaolla. [eli (/ 1 3) eikä 1/3]

## Standardikirjasto

Primitiivit:

* +, -, *, /
* =, <, >, <=, >=
* and
* eq?
* equal?
* set!
* set-car!
* set-cdr!
* car
* cdr
* cons
* list
* display
* newline
* apply
* eval, ei tosin tue ympäristön antamista parametrina
* floor
* modulo
* error
* number?
* string?
* symbol?
* pair?
* list?
* null?
* integer?
* rational?
* real?
* procedure?
* map
* length
* delay
* force
* read
* quote
* load

predef.scm-tiedosto ladataan nykyisestä hakemistosta automaattisesti. Mukana tuleva toteuttaa:

* not, odd?, even?, append, list-tail, memq, memv, member, eqv?,
* case (toteutettu makrona - aika kömpelösti kun ei quosiquoteakaan ole)

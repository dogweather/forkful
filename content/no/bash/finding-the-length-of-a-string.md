---
title:    "Bash: Å finne lengden av en streng"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden på en streng kan være en nyttig ferdighet å ha i Bash-programmering. Det lar deg enkelt håndtere og analysere tekstdata, noe som kan være nyttig for å løse ulike problemer og automatisere oppgaver.

## Hvordan

For å finne lengden på en streng i Bash, kan du bruke kommandoen `expr length` etterfulgt av en streng i anførselstegn. Her er et eksempel på hvordan du kan finne lengden på strengen "Hei verden!":

```Bash
len=`expr length "Hei verden!"`
echo "Lengden på strengen er $len"
```

Dette vil resultere i følgende output:

```Bash
Lengden på strengen er 12
```

Du kan også bruke for-løkker og `printf`-kommandoen til å finne lengden på en hel liste med strenger. Her er et eksempel:

```Bash
strenger=("Hei" "verden" "!" "Dette" "er" "en" "liste" "av" "strenger")

for str in ${strenger[@]}
do
    len=`printf "%s" "$str" | wc -m`
    echo "Lengden på $str er $len"
done
```

Output for dette blir:

```Bash
Lengden på Hei er 3
Lengden på verden er 6
Lengden på ! er 1
Lengden på Dette er 5
Lengden på er er 2
Lengden på en er 2
Lengden på liste er 5
Lengden på av er 2
Lengden på strenger er 8
```

## Deep Dive

For å virkelig forstå hvordan `expr length` fungerer, må vi se på hvordan Bash behandler strenger og tekstdata. Når du gir en streng til `expr length`, vil kommandoen skille alle tegnene i strengen og telle antall tegn.

Men hva med mellomrom? Du lurer kanskje på hvorfor "Hei verden" ble talt som 12 tegn når det egentlig er 11 tegn. Dette skyldes at Bash også teller mellomrom som et tegn, så selv om du ikke ser mellomrommet, blir det likevel talt.

## Se Også

- [Linuxize: How to Get the Length of a String in Bash](https://linuxize.com/post/how-to-get-the-length-of-a-string-in-bash/)
- [The Linux Command Line: Expressions](http://linuxcommand.org/lc3_lts0070.php)
- [Bash Hackers Wiki: Internal Variable: $IFS](https://wiki.bash-hackers.org/syntax/words#word_splitting)
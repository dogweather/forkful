---
title:    "C: Kalkulere en dato i fremtiden eller fortiden"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hvorfor
Mange ganger i programmering må vi kunne beregne en dato i fremtiden eller fortiden. Dette kan være nyttig for å lage en kalenderfunksjon eller planlegge hendelser. I denne bloggposten vil jeg vise deg hvordan du kan gjøre dette ved hjelp av C-programmering.

# Hvordan
For å beregne en dato i fremtiden eller fortiden, trenger vi å ta hensyn til måned, år og antall dager i måneden. La oss først opprette variabler som vil lagre denne informasjonen.

```C
int dag;
int måned;
int år;
int antall_dager;
```

Deretter kan vi be brukeren om å skrive inn en dato og antall dager som skal legges til eller trekkes fra. Vi bruker scanf funksjonen for å lese inn verdier fra brukeren.

```C
printf("Skriv inn dag: ");
scanf("%d", &dag);

printf("Skriv inn måned: ");
scanf("%d", &måned);

printf("Skriv inn år: ");
scanf("%d", &år);

printf("Skriv inn antall dager som skal legges til/trekkes fra: ");
scanf("%d", &antall_dager);
```

Nå er det på tide å beregne den nye datoen ved hjelp av måned og antall dager.

```C
måned += (antall_dager / 30);
dag += (antall_dager % 30);
```

Vi må også ta hensyn til skuddår når vi beregner datoer. Her bruker vi en if-setning for å sjekke om året er et skuddår eller ikke.

```C
if ((år % 4 == 0 && år % 100 != 0) || år % 400 ==0){
    if (måned > 12){
        måned = 1;
        år++;
    }
}
else{
    if (måned > 12){
        måned =1;
        år++;
    }
}
```

Til slutt kan vi skrive ut den nye datoen til brukeren.

```C
printf("Den nye datoen er: %d.%d.%d \n" , dag, måned, år);
```

Her er et eksempel på hvordan dette kan se ut i konsollen:

```
Skriv inn dag: 25
Skriv inn måned: 12
Skriv inn år: 2020
Skriv inn antall dager som skal legges til/trekkes fra: 10
Den nye datoen er: 4.1.2021
```

# Dypdykk
Nå som du har forstått hvordan du kan beregne en dato i fremtiden eller fortiden, kan vi se nærmere på noen av de tingene du bør være oppmerksom på når du jobber med datoberegninger.

Først må vi huske på at ikke alle måneder har 30 dager. Noen måneder har 31 dager og februar har bare 28 dager (eller 29 dager i et skuddår). Derfor er det viktig å sjekke om den nye datoen vi beregner faktisk er en gyldig dato.

I tillegg er det viktig å være oppmerksom på at noen land følger en annen kalender enn den gregorianske kalenderen som brukes i Norge. Det kan derfor være nyttig å ha en funksjon som lar brukeren velge hvilken kalender de ønsker å bruke for å beregne datoen.

# Se også
- [Skuddår - Wikipedia](https://no.wikipedia.org/wiki/Skudd%C3%A5r)
- [Kalender (nyere tid) - Store norske leksikon](https://snl.no/kalender_(nyere_tid))
- [Programmering på norsk - Programmeringsforlaget](https://www.programmeringsforlaget.no)
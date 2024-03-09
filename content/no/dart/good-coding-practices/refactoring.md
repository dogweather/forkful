---
title:                "Refaktorering"
date:                  2024-03-08T21:56:20.536942-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Refaktorering i Dart er prosessen med å restrukturere eksisterende kode uten å endre dens eksterne oppførsel, med mål om å forbedre dens interne struktur, lesbarhet og vedlikeholdbarhet. Programmerere refaktorerer ofte for å gjøre koden renere, lettere å forstå eller mer effektiv, noe som letter fremtidige modifikasjoner og reduserer sannsynligheten for feil.

## Hvordan:

### Eksempel 1: Omdøping og Utdraging av Metoder

Før refaktorering, kan du ha et stykke kode som blander ulike nivåer av abstraksjon eller ansvar, som å beregne en rabatt og deretter anvende den:

```dart
void main() {
  var pris = 100.0;
  var rabatt = 0.2;
  var sluttpris = pris - (pris * rabatt);
  print("Sluttpris: $sluttpris");
}
```

**Utdata:**
```
Sluttpris: 80.0
```

Etter refaktorering kan du trekke ut rabattberegningen til sin egen metode og gi den et meningsfylt navn:

```dart
void main() {
  var pris = 100.0;
  var rabatt = 0.2;
  var sluttpris = kalkulerSluttpris(pris, rabatt);
  print("Sluttpris: $sluttpris");
}

double kalkulerSluttpris(double pris, double rabatt) {
  return pris - (pris * rabatt);
}
```

**Utdata:**
```
Sluttpris: 80.0
```

Ved å trekke beregningen ut i en metode, har du nå en klart definert operasjon som kan gjenbrukes, testes uavhengig og enkelt modifiseres.

### Eksempel 2: Forenkling av Betingede Uttrykk

Før refaktorering kan betingede uttalelser være overdrevent komplekse eller vanskelige å lese:

```dart
void main() {
  var kundetype = "vanlig";
  double rabatt;
  
  if (kundetype == "vanlig") {
    rabatt = 0.05;
  } else if (kundetype == "medlem") {
    rabatt = 0.1;
  } else {
    rabatt = 0.0;
  }

  print("Rabatt: $rabatt");
}
```

**Utdata:**
```
Rabatt: 0.05
```

Etter refaktorering, vurdere å bruke et kart for klarere struktur og enklere oppdateringer eller utvidelser til kundetyper og rabatter:

```dart
void main() {
  var kundetype = "vanlig";
  var rabatter = {
    "vanlig": 0.05,
    "medlem": 0.1,
    "ingen": 0.0,
  };

  var rabatt = rabatter[kundetype] ?? 0.0;
  print("Rabatt: $rabatt");
}
```

**Utdata:**
```
Rabatt: 0.05
```

Denne refaktoreringen gjør ikke bare koden mer konsis, men kapsler også logikken for å bestemme rabatter på en måte som er lettere å forstå og vedlikeholde.

### Tredjepartsbiblioteker for Refaktorering

Når det kommer til refaktorering i Dart, spesielt innen Flutter-apper, er [Dart DevTools](https://dart.dev/tools/dart-devtools)-pakken uvurderlig. Den inkluderer ytelsesverktøy, en widget-inspektør og en kildekode-nivå debugger. Selv om det ikke er et tredjepartsbibliotek, blir Dart DevTools ofte brukt sammen med biblioteker som `flutter_bloc` for ryddig håndtering av tilstand på en måte som er gunstig for refaktorering for forbedret modularitet og lesbarhet. Dessverre, på grunn av omfanget av dette innlegget, vil spesifikke kodeeksempler som bruker tredjepartsbiblioteker ikke bli gitt her, men utviklere oppfordres til å utforske disse verktøyene for å forbedre refaktoreringsprosessen i sine Dart/Flutter-applikasjoner.

---
title:    "Elm: Kontrollera om en katalog finns"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar är en viktig del av att skriva robusta program i Elm. Genom att utföra denna kontroll kan du undvika att programmet kraschar om en angiven mapp inte finns, vilket kan spara tid och frustration i längden.

## Så här gör du

För att kontrollera om en mapp existerar i Elm, kan du använda den inbyggda funktionen `Directory.exists`. Denna funktion tar in en sträng som representerar sökvägen till mappen och returnerar en `Result` typ. Om mappen finns returneras `Ok`, annars returneras ett felmeddelande.

```Elm
import Directory

Directory.exists "mapp/mapp2"
--> Ok

Directory.exists "fel/mapp"
--> Err "Cannot find directory: fel/mapp"
```

## Djupdykning

I Elm är utforskning av filsystemet begränsat på grund av säkerhetsfunktioner. Detta är för att hålla språket säkert och förhindra att användare oavsiktligt ändrar filer på deras dator. Därför kan bara innehållet i den aktuella mappen och undermappar nås genom `Directory` -modulen.

Om du vill ha mer avancerade metoder för att hantera filsystemet i Elm kan du överväga att använda bibliotek som `elm-file` eller `elm-filepicker`. Dessa bibliotek tillåter dig att göra mer komplexa åtgärder med filsystemet, som att skapa och ta bort mappar eller välja filer från användarens dator.

## Se även

- [Elm Language Documentation](https://elm-lang.org/docs)
- [Elm-lang Discuss Forum](https://discourse.elm-lang.org/)
- [elm-file](https://package.elm-lang.org/packages/Azazdeaz/elm-file/latest/)
- [elm-filepicker](https://package.elm-lang.org/packages/arowM/elm-filepicker/latest/)
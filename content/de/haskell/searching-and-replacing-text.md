---
title:    "Haskell: Suchen und Ersetzen von Text"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Warum

Textsuche und -ersetzung ist eine wichtige Technik beim Programmieren. Sie ermöglicht es, bestimmte Zeichenfolgen in einem Text zu finden und zu ersetzen, was die Arbeit effizienter und schneller macht. In diesem Blogbeitrag lernen Sie, wie Sie Textsuche und -ersetzung in Haskell verwenden können, um Ihre Programmieraufgaben zu vereinfachen.

## Wie geht das?

Um Textsuche und -ersetzung in Haskell durchzuführen, benötigen Sie die Funktion `substitute` aus dem Modul `Data.List`. Diese Funktion akzeptiert drei Argumente: das zu ersetzende Muster, die Ersatzzeichenfolge und den Text, in dem die Suche durchgeführt werden soll. Ein Beispielcode sieht folgendermaßen aus:

```Haskell
import Data.List

text = "Hallo, wie geht es dir?"

substitute "dir" "mir" text
```

Die Ausgabe dieses Codes lautet: "Hallo, wie geht es mir?" Wie Sie sehen, wurde das Wort "dir" durch "mir" ersetzt.

Neben `substitute` gibt es auch noch andere nützliche Funktionen wie `replace` und `replaceAll`. Diese können ebenfalls zum Suchen und Ersetzen von Text verwendet werden.

## Tiefere Einblicke

Die Funktionen `substitute`, `replace` und `replaceAll` in Haskell verwenden unter der Haube das Konzept der regulären Ausdrücke. Reguläre Ausdrücke sind Muster, mit denen Sie nach bestimmten Zeichenfolgen in einem Text suchen können. Sie können auch Platzhalter und Quantoren verwenden, um Ihre Suche noch spezifischer zu gestalten.

Eine einfache Suche und Ersetzung kann beispielsweise mit dem regulären Ausdruck `dir` durchgeführt werden. Dabei werden alle Vorkommen von "dir" im Text durch "mir" ersetzt. Mit dem regulären Ausdruck `[0-9]+` können Sie beispielsweise alle Zahlen im Text finden und ersetzen.

Um sich mit regulären Ausdrücken vertraut zu machen, empfehlen wir Ihnen, unsere anderen Blogbeiträge zum Thema zu lesen oder Online-Ressourcen zu nutzen.

## Siehe auch

- [Learn You a Haskell for Great Good! - Kapitel 8: Muster zusammenfügen](http://learnyouahaskell.com/functionally-solving-problems#an-intro-to-regular-expressions)
- [Hoogle - Dokumentation zu `substitute`, `replace` und `replaceAll`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#g:4)
- [Regex Tutorial - Eine interaktive Anleitung zu regulären Ausdrücken](https://regexone.com/)
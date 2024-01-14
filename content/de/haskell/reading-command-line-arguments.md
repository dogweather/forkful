---
title:    "Haskell: Das Lesen von Befehlszeilen-Argumenten"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist eine grundlegende Fähigkeit, die jeder Entwickler in Haskell beherrschen sollte. Durch das Verstehen dieser Funktion können wir unsere Anwendungen anpassen und auf vielfältige Weise interagieren.

## Wie geht das?

In Haskell bietet die `System.Environment`-Bibliothek verschiedene Funktionen, um Befehlszeilenargumente zu lesen. Die gebräuchlichste ist `getArgs`, die eine Liste von `String`-Werten zurückgibt, die die Befehlszeilenargumente enthält.

Ein einfaches Beispiel, um die Befehlszeilenargumente auszugeben könnte folgendermaßen aussehen:

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn $ "Die Befehlszeilenargumente sind: " ++ show args
```

Wenn wir nun dieses Programm mit dem Befehl `runhaskell args.hs arg1 arg2` ausführen, würde als Output erscheinen:

```
Die Befehlszeilenargumente sind: ["arg1", "arg2"]
```

## Tiefes Eintauchen

Nun, da wir wissen, wie wir Befehlszeilenargumente lesen können, können wir auch tiefer eintauchen und uns mit verschiedenen Aspekten dieser Funktion auseinandersetzen. Ein wichtiger Punkt ist z.B. die Reihenfolge der Befehlszeilenargumente. In Haskell werden die Befehlszeilenargumente in der gleichen Reihenfolge wie sie eingegeben wurden in der Liste zurückgegeben. Dies kann hilfreich sein, wenn wir bestimmte Argumente benötigen, die in einer bestimmten Reihenfolge eingegeben wurden.

Eine weitere wichtige Funktion ist `getProgName`, die den Programmnamen zurückgibt, mit dem das Programm gestartet wurde. Dies kann nützlich sein, wenn wir unser Programm abhängig von seinem Namen unterschiedliche Aktionen ausführen lassen wollen.

Es ist auch möglich, genauere Daten über die Argumente zu erhalten, z.B. durch die Verwendung von `lookupEnv`, mit der wir überprüfen können, ob ein bestimmtes Argument übergeben wurde oder nicht.

## Siehe auch

- [Offizielle Dokumentation zu `System.Environment`](https://www.haskell.org/onlinereport/haskell2010/haskellch18.html#x25-25000018)
- [Tutorial zum Umgang mit Befehlszeilenargumenten in Haskell](https://jkachmar.github.io/posts/2017-01-01-parsing-command-line-arguments-in-haskell.html)
- [Haskells `System.Environment`-Bibliothek auf Hackage](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html)
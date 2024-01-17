---
title:                "文字列の補間"
html_title:           "PowerShell: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何 & なぜ?

文字列を補間することは、プログラマーが変数や式を文中に埋め込むことを意味します。これにより、より動的で読みやすいコードを作成することができます。

プログラマーは、変数や式を手動で文に追加するのではなく、コードをより簡潔にし、ミスを防ぐために、変数や式を自動的に補間することを好みます。

## 方法:

「$」記号を使用して、変数または式を文字列に埋め込むことができます。以下の例をご覧ください。

```PowerShell
$name = "John"
Write-Host "こんにちは、$nameさん。今日はいい天気ですね。"
```

出力:

```
こんにちは、Johnさん。今日はいい天気ですね。
```

## 深堀り:

文字列を補間する概念は、プログラミングの歴史を通じて変化してきました。以前は、変数や式を手動で文に追加する必要がありましたが、現在のプログラミング言語は補間をより簡単に実現するようになりました。

変数や式を文字列に埋め込む代替方法として、文字列結合があります。これは、複数の文字列を結合して新しい文字列を作成する方法です。しかし、補間はより効率的かつ読みやすい方法とされています。

補間は、大文字と小文字を区別することなく変数や式を認識しますが、文字列結合は品質が優れない場合があります。そのため、補間はより信頼性の高い方法と言えます。

## 関連情報:

- [Microsoft Docs - 文字列リテラル](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7)
- [Tech on the Net - PowerShellの文字列補間](https://www.techonthenet.com/powershell/variables/interpolate.php)
- [PowerShell Guru - 文字列補間のパフォーマンス](https://www.powershellguru.com/benchmarks-string-interpolation-vs-string-concatenation/)
---
title:                "C#: 「文字列の先頭を大文字化する」"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

英語のテキストでは、単語の最初の文字を大文字にすることは非常に一般的です。例えば、文章のタイトルや人名などが該当します。このような場面で文字列を正しく大文字にすることは非常に重要です。コードを使ってこれを行う方法を学ぶことで、より効率的に作業を進めることができます。

## 方法

大文字にするには、C#の```ToUpper()```メソッドを使用します。以下の例をご覧ください。

```C#
string text = "hello world";
string capitalizedText = text.ToUpper();
Console.WriteLine(capitalizedText);

// Output: HELLO WORLD
```

以上のように、文字列を```ToUpper()```メソッドで大文字にすることができます。これは、入力された文字列を大文字にするだけでなく、トリムやパディングなどの文字列の編集も自動的に行ってくれます。また、英語のアルファベットだけでなく、日本語のようなマルチバイト文字も正しく処理してくれます。

## 深堀り

C#では、文字列の大文字化にはほかにも様々なメソッドやライブラリがあります。例えば、```ToUpperInvariant()```メソッドはカルチャに依存せず、常に英語のみを大文字にします。また、```CultureInfo```クラスを使用することで、特定の地域や言語のルールに従って文字列を大文字にできます。これらのメソッドやクラスを組み合わせることで、より柔軟に文字列を編集することができます。

## See Also

- [String.ToUpper Method (System) - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-5.0)
- [Culture-Sensitive String Operations - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/culture-sensitive-string-operations)
- [CultureInfo Class - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-5.0)
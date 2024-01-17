---
title:                "文字列の大文字化"
html_title:           "PowerShell: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##何か & 何故？
ストリングを大文字にすることは、プログラマーにとって非常によく使われる作業です。これは、文字列を大文字にすることで、文字列を識別しやすくしたり、見やすくしたりするために行われます。プログラマーは、様々なプログラム内で文字列を扱うことが多いため、大文字にすることでコードの可読性を高めることができます。

##やり方：
**PowerShellを使ったコード例と出力サンプル**

```PowerShell
# 文字列を大文字に変換する方法

# 文字列を変数に格納する
$string = "hello world"

# 大文字に変換する
$string.ToUpper()

# 出力結果：
HELLO WORLD
```

```PowerShell
# 文字列がすでに大文字の場合、変換されないことを確認する

# 大文字の文字列を変数に格納する
$string = "HELLO WORLD"

# 大文字に変換する
$string.ToUpper()

# 出力結果：
HELLO WORLD
```

```PowerShell
# 文字列の最初の文字だけを大文字にする方法

# 文字列を変数に格納する
$string = "hello world"

# 最初の文字のみを大文字に変換する
$string.Substring(0,1).ToUpper() + $string.Substring(1)

# 出力結果：
Hello world
```

##詳細を深く掘り下げる
**歴史的文脈：**
文字列の大文字変換は、古くからコンピューターの世界で広く使われてきました。初期のコンピューターは文字列の大文字と小文字を区別することができなかったため、文字列の比較や識別を行う際に大文字変換が必要となりました。また、プログラミング言語の中にも、大文字変換に関する組み込みの関数やメソッドがあります。

**代替手段：**
文字列の大文字変換は、PowerShellの他にも多くのプログラミング言語で行うことができます。例えば、PythonやJavaでも同様に文字列を大文字に変換することができます。ただし、それぞれの言語の構文やメソッドは異なるため、コードを書く際には注意が必要です。

**実装の詳細：**
PowerShellでは、文字列を大文字に変換する方法として、組み込みのToUpper()メソッドを使用します。このメソッドは、文字列オブジェクトのメソッドの一つであり、全ての文字列に対して使用することができます。また、ToUpper()メソッドは文字列の元の値を変更せずに、新しい文字列を返すため、元の文字列の大文字変換は安全に行うことができます。

##関連情報を参照
- [Microsoft Documentation - String.ToUpper()メソッド](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=netframework-4.8)
- [C# Corner - Convert String to Uppercase in PowerShell](https://www.c-sharpcorner.com/blogs/convert-string-to-uppercase-in-powershell)
- [PowerShell Gallery - String Manipulation](https://www.powershellgallery.com/packages/StringManipulation/1.0/Content/lib%5Cstringfunctions.ps1)
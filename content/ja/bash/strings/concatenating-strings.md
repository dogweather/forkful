---
date: 2024-01-20 17:34:22.512390-07:00
description: "How to: (\u3084\u308A\u65B9) \u6700\u521D\u306B\u3001Bash\u306B\u3088\
  \u308B\u6587\u5B57\u5217\u306E\u9023\u7D50\u306F\u975E\u5E38\u306B\u5358\u7D14\u3067\
  \u3059\u30021989\u5E74\u306B\u767B\u5834\u3057\u305FBash\u306F\u3001Unix\u30B7\u30A7\
  \u30EB\u306E\u4F1D\u7D71\u3092\u5F15\u304D\u7D99\u304E\u3064\u3064\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30DF\u30F3\u30B0\u306E\u7C21\u5358\u3055\u3092\u6539\u5584\u3057\u307E\
  \u3057\u305F\u3002\u4ECA\u65E5\u3067\u3082\u3001\u3053\u306E\u30B7\u30F3\u30D7\u30EB\
  \u306A\u9023\u7D50\u306F\u30B9\u30AF\u30EA\u30D7\u30C8\u306E\u8AAD\u307F\u3084\u3059\
  \u3055\u3068\u4FDD\u5B88\u6027\u306B\u8CA2\u732E\u3057\u3066\u3044\u307E\u3059\u3002\
  \ \u4ED6\u306E\u65B9\u6CD5\u3068\u3057\u3066\u3001`printf` \u3084\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.193385-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u6700\u521D\u306B\u3001Bash\u306B\u3088\u308B\u6587\
  \u5B57\u5217\u306E\u9023\u7D50\u306F\u975E\u5E38\u306B\u5358\u7D14\u3067\u3059\u3002\
  1989\u5E74\u306B\u767B\u5834\u3057\u305FBash\u306F\u3001Unix\u30B7\u30A7\u30EB\u306E\
  \u4F1D\u7D71\u3092\u5F15\u304D\u7D99\u304E\u3064\u3064\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30DF\u30F3\u30B0\u306E\u7C21\u5358\u3055\u3092\u6539\u5584\u3057\u307E\u3057\u305F\
  \u3002\u4ECA\u65E5\u3067\u3082\u3001\u3053\u306E\u30B7\u30F3\u30D7\u30EB\u306A\u9023\
  \u7D50\u306F\u30B9\u30AF\u30EA\u30D7\u30C8\u306E\u8AAD\u307F\u3084\u3059\u3055\u3068\
  \u4FDD\u5B88\u6027\u306B\u8CA2\u732E\u3057\u3066\u3044\u307E\u3059."
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

## How to: (やり方)
```Bash
# 直接つなげる
string1="Hello,"
string2=" World!"
greeting=$string1$string2
echo $greeting # 出力: Hello, World!

# 変数を使わずにつなげる
echo "Concatenating " "strings " "is " "fun!" # 出力: Concatenating strings is fun!

# {}を使って明確にする
name="Taro"
echo "Your name is ${name}san." # 出力: Your name is Tarosan.
```

## Deep Dive (深掘り)
最初に、Bashによる文字列の連結は非常に単純です。1989年に登場したBashは、Unixシェルの伝統を引き継ぎつつ、プログラミングの簡単さを改善しました。今日でも、このシンプルな連結はスクリプトの読みやすさと保守性に貢献しています。

他の方法として、`printf` や `echo` などのコマンドでも文字列を連結できますが、変数だけで連結する方がシンプルで効率的です。演算子や関数を使用せずとも、Bashは変数を直接つなげることで文字列を連結させます。内部実装では、Bashは文字列を連結する際に新しいメモリ領域を割り当て、元の文字列を新しい領域にコピーしています。

## See Also (関連情報)
- Bash String Manipulation: [https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- Advanced Bash-Scripting Guide: [https://tldp.org/LDP/abs/html/](https://tldp.org/LDP/abs/html/)
- Bash Programming Introduction HOWTO: [https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)

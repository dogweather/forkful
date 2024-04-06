---
date: 2024-01-20 17:50:18.985885-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.597447-06:00'
model: gpt-4-1106-preview
summary: "(\u5B9F\u8DF5\u65B9\u6CD5) \u6587\u5B57\u5217\u88DC\u9593\u306F\u53E4\u304F\
  \u304B\u3089\u30B7\u30A7\u30EB\u30B9\u30AF\u30EA\u30D7\u30C8\u306B\u304A\u3051\u308B\
  \u57FA\u672C\u6A5F\u80FD\u3067\u3001\u52B9\u7387\u7684\u306A\u30B3\u30FC\u30C9\u4F5C\
  \u6210\u306B\u5F79\u7ACB\u3061\u307E\u3059\u3002Bash\u4EE5\u524D\u306F`expr`\u30B3\
  \u30DE\u30F3\u30C9\u3092\u4F7F\u7528\u3057\u307E\u3059\u304C\u3001\u4ECA\u3067\u306F\
  `$()`\u3084`${}`\u69CB\u6587\u304C\u4F7F\u308F\u308C\u3066\u3044\u307E\u3059\u3002\
  \u3053\u308C\u3089\u306F\u5B50\u30D7\u30ED\u30BB\u30B9\u3092\u4F5C\u3089\u305A\u306B\
  \u51E6\u7406\u3067\u304D\u308B\u305F\u3081\u52B9\u7387\u7684\u3067\u3059\u3002\u4EE3\
  \u308F\u308A\u306B\u30C0\u30D6\u30EB\u30AF\u30AA\u30FC\u30C8\u3092\u4F7F\u3046\u3068\
  \u3001\u5909\u6570\u3084\u30B3\u30DE\u30F3\u30C9\u51FA\u529B\u304C\u5C55\u958B\u3055\
  \u308C\u307E\u305B\u3093\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

## How to: (実践方法)
```Bash
# 変数に値を割り当てます
greeting="こんにちは"
name="世界"

# 文字列補間を使って変数を出力します
echo "${greeting}、${name}！"

# 数式の結果も埋め込むことができます
a=5
b=10
echo "合計は: $(($a + $b))"
```
出力:
```
こんにちは、世界！
合計は: 15
```

## Deep Dive (掘り下げ)
文字列補間は古くからシェルスクリプトにおける基本機能で、効率的なコード作成に役立ちます。Bash以前は`expr`コマンドを使用しますが、今では`$()`や`${}`構文が使われています。これらは子プロセスを作らずに処理できるため効率的です。代わりにダブルクオートを使うと、変数やコマンド出力が展開されません。

## See Also (参照)
- Bash Manual: [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- Advanced Bash-Scripting Guide: [ABS Guide](https://www.tldp.org/LDP/abs/html/)

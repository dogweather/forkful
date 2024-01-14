---
title:    "Haskell: 標準エラーへの書き込み"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

日本語版Haskellプログラミングブログポスト：標準エラー出力の書き方

## Why
プログラミングにおいてデバッグは欠かせない作業です。その際、標準エラー出力を使うことでエラーメッセージやデバッグ情報をコンソール上に表示できます。

## How To
コードブロックを使って、実際のコーディング例と出力を紹介します。

```Haskell
main = do
    putStrLn "Hello World!" -- 標準出力に表示される
    hPutStrLn stderr "Error: Something went wrong..." -- 標準エラー出力に表示される
```

もしこのコードが正しく実行された場合は、コンソール上には以下のように表示されるでしょう。

```
Hello World!
Error: Something went wrong...
```

## Deep Dive
標準出力と標準エラー出力は似ていますが、プログラム内での役割が異なります。標準出力はプログラムの実行結果や情報を表示するための標準的な手段です。一方、標準エラー出力はエラーメッセージやデバッグ情報を表示するためのもので、プログラムの実行において重要な情報を提供する役割を持ちます。

標準エラー出力は、プログラムがエラーを検出した際に自動的に表示されるものではありません。プログラム内で明示的に指定する必要があります。また、標準エラー出力の内容をリダイレクトすることで、プログラムの実行結果とエラーメッセージを分けて表示することもできます。

## See Also
- [Haskellプログラミング](https://www.haskell.org/)
- [標準エラー出力についてのリファレンス](https://www.ibm.com/support/knowledgecenter/en/SSLTBW_2.3.0/com.ibm.zos.v2r3.bpxbd00/strerr.htm)
---
title:                "スタンダードエラーへの書き込み"
html_title:           "C: スタンダードエラーへの書き込み"
simple_title:         "スタンダードエラーへの書き込み"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ
標準エラーに書き込む必要があるのかを説明します。

標準エラーに書き込むと、プログラムが実行される際に発生するエラーを確認することができます。これにより、プログラムのデバッグや問題の特定が容易になります。

## 使い方

以下のように、```C ... ```のコードブロックを使用して、標準エラーにメッセージを出力することができます。

```C
fprintf(stderr, "エラーが発生しました。");
```

このようにすることで、プログラムが実行される際に発生するエラーをターミナル上に表示することができます。

## 深堀り

標準エラーは、Cの標準出力とは異なり、バッファリングされないため、プログラムの実行中にすぐに出力を受け取ることができます。また、```stderr```を使用することで、ファイルにエラーメッセージを出力することもできます。

## 詳しくは

- [標準エラー | Microsoft Docs](https://docs.microsoft.com/ja-jp/cpp/c-runtime-library/reference/standard-error-stderr)
- [C言語でのエラー処理の方法 | Qiita](https://qiita.com/ysk24ok/items/888a2db5d07b09680532)
- [fprintf | 日本語のリファレンスマニュアル](https://9cguide.appspot.com/06-09.shtml)

## 関連リンク

- [C言語の基礎 | TechAcademyマガジン](https://techacademy.jp/magazine/18553)
- [プログラミング初心者なので、まずはC言語を学びましょう | paiza開発日誌](https://paiza.hatenablog.com/entry/2014/02/28/%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0%E5%88%9D%E5%BF%83%E8%80%85%E3%81%AA%E3%81%AE%E3%81%A7%E3%80%81%E3%81%BE%E3%81%9A%E3%81%AFC%E8%A8%80%E8%AA%9E%E3%82%92%E5%AD%A6)
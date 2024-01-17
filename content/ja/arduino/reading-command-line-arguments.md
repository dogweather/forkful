---
title:                "コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
html_title:           "Arduino: コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
simple_title:         "コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

コマンドライン引数を読み取るとは何かと、プログラマーがそれを行う理由は何かを説明します。

コマンドライン引数を読み取るとは、プログラムが起動時にユーザーから与えられたコマンドやデータのことを指します。プログラマーはコマンドライン引数を読み取ることで、ユーザーの入力に応じた処理を行うことができます。

## どのように：

下の```Arduino ... ```コードブロック内に、コーディングの例とサンプルの出力を示します。

```
Arduinoコードをここに入力してください
```

コマンドライン引数を読み取るには、```Arduino Code```を使用します。例えば、```Arduino Code```を使用して、ユーザーからの入力を文字列として受け取ることができます。

```
Arduinoコードをここに入力してください

int main(int argc, char *argv[]) {

    String user_input = String(argv[1]);

    Serial.println(user_input);

    return 0;
}

```

入力した文字列がシリアルモニターに表示されることを確認しましょう。

## 詳細な情報：

コマンドライン引数を読み取る方法については、Arduinoのドキュメンテーションを参照することができます。また、別の方法として、ライブラリを使用することもできます。それぞれの方法には、利点や欠点がありますので、自分のプログラムに合った最適な方法を選択しましょう。

コマンドライン引数の歴史や、他の言語での実装方法も調べてみると、より深く理解することができます。

## 関連リンク：

- [Arduino Command Line Arguments ガイド](https://www.arduino.cc/en/Main/CommandLineArguments)
- [Arduino Stringクラス](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [C言語でのコマンドライン引数の読み取り方](https://www.ibm.com/support/knowledgecenter/ja/ssw_ibm_i_73/rtref/cgets.htm)
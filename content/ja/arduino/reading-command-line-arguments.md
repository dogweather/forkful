---
title:                "Arduino: コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

今回は、Arduinoプログラミングでコマンドライン引数を読み込む方法について解説します。コマンドライン引数は、プログラムの実行時に追加の情報を与えるために使用されます。例えば、特定のセンサーのデータを取得したい場合や、アクションを実行する際のパラメーターを指定したい場合などに使用することができます。この記事を読むことで、あなたのプログラムにコマンドライン引数を使用することができるようになります。

## 方法

まずは、コードを見てみましょう。以下のコードブロック内に、コマンドライン引数を読み込むために必要なコードを記述します。

```Arduino 
int main(int argc, char *argv[]) {
  // コマンドライン引数が与えられたか確認
  if (argc > 1) {
    // 第一引数を取得
    int arg1 = atoi(argv[1]);
    // 第二引数を取得
    char* arg2 = argv[2];
    // ここで取得した引数を使用して、プログラムを実行する
  }
}
```

上記のコードを見るとわかるように、まず`int main(int argc, char *argv[])`という関数を使用して、コマンドライン引数を読み込みます。`argc`は、コマンドライン引数の数を表し、`argv[]`は引数を格納する配列です。そして、`if`文を使用して引数が与えられたかを確認し、与えられた場合には`argv[]`の中身を使用してプログラムを実行します。

## 詳細を掘り下げる

では、もう少し詳しくコードを見てみましょう。上記のコードでは、第一引数を`int`型に変換して`arg1`に格納していますが、第二引数の`argv[2]`は`char*`型であることに注意してください。つまり、第二引数には文字列が入る可能性があるので、`atoi()`ではなく`char*`をそのまま`arg2`に格納しています。このように、引数の型によって取得方法が異なることにも注意してください。また、`argv[]`は配列ということもあり、複数の引数を取得することもできます。必要に応じて、`for`ループなどを使用して処理を行ってください。

## 関連情報はこちら

- [Arduinoの公式ドキュメント - コマンドライン引数の取得方法](https://www.arduino.cc/en/Tutorial.CommandLineArguments)
- [Qiitaの記事 - Arduinoでコマンドライン引数を使う方法](https://qiita.com/SaitoYutaka/items/554d75e7187fc7776563)
- [TechAcademy Magazine - Arduinoプログラミングの基礎](https://techacademy.jp/magazine/19955)
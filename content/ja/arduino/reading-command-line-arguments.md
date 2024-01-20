---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Arduinoプログラミング: コマンドライン引数の読み取り

## 何と何のため？
コマンドライン引数はプログラムが起動時に受け取るパラメータの一つです。これにより、プログラマーはプログラムの挙動を柔軟に制御できます。

## 実践方法
以下はArduinoでコマンドライン引数を読み取るソースコードの例です。

```Arduino
int argCount;
char **args;
void setup() {
  Serial.begin(9600);
  args = CommandLineArgs.get(&argCount);
}

void loop() {
  for (int i = 0; i < argCount; i++) {
    Serial.println(args[i]);
  }
}
```

このプログラムを実行すると、シリアルモニターにコマンドライン引数が表示されます。

## ディープダイブ
コマンドライン引数はUNIXのシェルスクリプトに由来し、プログラムの初期セットアップを制御するために使われます。Arduinoではエミュレータやシミュレータで引数の授受が可能です。一方、ハードウェアで直接コマンドライン引数を読み込む機能はありません。

代替手段としてArduinoでは、インタラクティブなパラメータ設定のためにシリアル入力を使用することが一般的です。また、SDカードからの設定ファイルの読み込みも可能です。

コマンドライン引数の受け取りはArduinoの`main`関数で発生します。それらは配列に格納され、プログラム全体で使用できます。

## 参照情報
以下リンクで関連する情報をさらに詳しく学べます。

- [Arduino.cc: Serial Input Basics](https://forum.arduino.cc/index.php?topic=396450)
- [Arduino Stack Exchange: How to read command line arguments](https://arduino.stackexchange.com/questions/333/how-to-read-command-line-arguments-in-arduino)
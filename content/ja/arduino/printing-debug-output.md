---
title:                "デバッグ出力の印刷"
html_title:           "Arduino: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why（なぜ）

Arduinoのデバッグ出力を使うことで、コードの動作をより詳細に理解し、問題を追跡することができます。また、コードの改善や最適化にも役立ちます。

## How To（やり方）

デバッグ出力を使うには、Serialモニターを使います。以下のコードを`setup()`関数内に追加すると、Serial通信を開始できます。

```Arduino
Serial.begin(9600);
```

`9600`は、通信速度（ボー）を表します。`loop()`関数内でデバッグしたい値やメッセージを`Serial.print()`や`Serial.println()`を使って出力することができます。

```Arduino
Serial.print("現在の値は：");
Serial.println(value);
```

これらの出力は、Serialモニターに表示されます。必要に応じて、`Serial.begin()`の速度を変更することもできます。

## Deep Dive（詳しい情報）

デバッグ出力は、プログラミングで最も一般的なツールの一つです。コードを実行する際に、変数の値やメッセージを表示することで、コードがどのように動作し、どのように変数が変化するかを確認することができます。

Arduinoでは、Serial通信が実現されており、`Serial.print()`や`Serial.println()`を使ってデータを送信することができます。また、Serialモニターで受信したデータを`Serial.read()`を使って読み取ることもできます。

デバッグ出力を使うことで、コードの動作に関する情報を収集し、問題を解決することができます。また、コードの最適化や改善にも役立ちます。

## See Also（関連リンク）

- [Arduino公式サイト](https://www.arduino.cc/)
- [Serial通信についてのチュートリアル](https://www.arduino.cc/reference/en/language/functions/communication/serial/)

**Note: この記事はGitHubのリポジトリに掲載されています。修正や改善のプルリクエストを歓迎します。**
---
title:    "Arduino: 標準エラーへの書き込み"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

#なぜ
Arduinoプログラミングを学ぶ際に、エラーを記述することは非常に重要です。エラーを書くことで、問題を特定し、修正するための手がかりを得ることができます。そのため、Arduinoプログラミングの初心者にとっても、熟練したプログラマーにとっても、エラーを記述することは欠かせないスキルです。

##やり方
まずは、Arduinoのシリアルモニターを開きます。そこに表示されるメッセージを読むことで、どのようなエラーが発生したのかを知ることができます。また、下記のように`Serial.println()`を使用することで、自分でエラーメッセージを表示することもできます。

```Arduino
Serial.println("エラーのメッセージ");
```

もしコードが複雑で、正しい箇所を特定するのが難しい場合、`Serial.println()`を使用して、特定の変数や値を表示することで、どこで問題が起きているかを把握することができます。例えば、下記のように書くことで、`val`という変数の値を表示することができます。

```Arduino
Serial.println(val);
```

##ディープダイブ
エラーを記述するには、`Serial.println()`だけでなく、`Serial.print()`や`Serial.printf()`を使用することもできます。これらの関数を使用することで、エラーの情報をフォーマットすることができます。また、`Serial.begin()`関数を使用することで、シリアル通信のボーレートを設定することもできます。詳細情報については、Arduinoのドキュメンテーションを参照してください。

#参考リンク
- [Arduinoドキュメンテーション](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino Forum - エラーを記述する方法について](https://forum.arduino.cc/index.php?topic=692218.msg4671609#msg4671609)
- [日本語のArduinoチュートリアル](https://www.musashinodenpa.com/arduino/ref/index.php/%E3%82%A8%E3%83%A9%E3%83%BC%E3%83%A1%E3%83%83%E3%82%BB%E3%83%BC%E3%82%B8)
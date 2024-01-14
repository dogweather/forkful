---
title:                "Arduino: デバッグ出力の印刷"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

#なぜ
デバッグ出力をプリントすることに参加する理由を説明していること。

##方法
プリントしてデバッグする方法について説明するために、いくつかのコーディング例を紹介する。次に、実際の出力例を示すために、 " ```Arduino…``` "コードブロック内にサンプル出力を示す。

```Arduino
//コード例1：変数の値をプリントする
int x = 5;
Serial.print("変数xの値は： ");
Serial.println(x);

出力：
変数xの値は： 5

//コード例2：forループで配列の要素をプリントする
int array[3] = {1, 2, 3};
for(int i = 0; i < 3; i++){
  Serial.print("配列の要素");
  Serial.print(i);
  Serial.print("の値は： ");
  Serial.println(array[i]);
}

出力：
配列の要素0の値は： 1
配列の要素1の値は： 2
配列の要素2の値は： 3
```

##ディープダイブ
デバッグ出力はコードを実行している間にコンピューターの内部の状態を表示するための重要なツールであり、問題を特定するのに役立ちます。デバッグ出力を使用することで、プログラムが期待どおりに動作するかどうかを確認し、必要に応じて変更を加えることができます。

デバッグ出力を表示するには、 `Serial`オブジェクトの`print（）`や`println（）`メソッドを使用します。これらのメソッドの引数には、文字列または変数を含めることができます。`print（）`メソッドは引数をプリントし、改行なしで次の出力を続けます。一方、`println（）`メソッドは引数をプリントし、その後改行します。

デバッグ出力にはさまざまな用途があります。たとえば、変数の値を表示して、プログラムが正しく動作しているかどうかを確認できます。また、条件分岐の処理の中でデバッグ出力を使用して、特定の条件が実行されたことを確認することができます。

デバッグ出力を使用して問題を特定したら、プログラムから削除することを忘れないようにしましょう。デバッグ出力はコンピューターのパフォーマンスに影響を与える可能性があるため、不要になったら削除することが重要です。

#参考
- [Arduinoでデバッグする方法](https://www.arduino.cc/en/Tutorial/CommandParse)
- [デバッグ出力のベストプラクティス](https://www.arduino.cc/en/Reference/StartupDebug)
- [第2回：Arduinoでシリアル通信をする（デバッグ出力の方法）](https://deviceplus.jp/hobby/entry003/)
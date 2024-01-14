---
title:    "Arduino: ディレクトリが存在するかどうかを確認する"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ
ディレクトリが存在するかどうかを確認することは、Arduinoプログラミングで非常に重要です。デバイスやセンサーからのデータを保存するために、正しいフォルダーにアクセスする必要があります。そのため、ディレクトリの存在をチェックすることは、データ管理や正しいプログラムの実行にとって必要不可欠です。

## 方法
Arduinoでは、ディレクトリをチェックするための便利な関数が用意されています。それは「SD.exists（）」です。以下の例では、SDカードに「sample.txt」というファイルが存在するかどうかをチェックしています。

```
Arduino ...
if(SD.exists("sample.txt")){
  Serial.println("sample.txt exists in the SD card.");
}
else{
  Serial.println("sample.txt does not exist.");
}
```

上記のコードを実行すれば、シリアルモニターに「sample.txt exists in the SD card.」というメッセージが表示されるはずです。

## ディープダイブ
SD.exists（）関数は、ファイルではなくディレクトリをチェックすることもできます。SDカード内にフォルダーがあるかどうかをチェックするには、「SD.exists（「/ samplefolder」）」のように、フォルダーパスを指定します。

また、SD.exists（）関数は、複数のファイルやフォルダーを一度にチェックすることもできます。たとえば、「SD.exists（「/ samplefolder / sample1.txt」、「/ samplefolder / sample2.txt」、「/ samplefolder2」）」のように、引数に複数のパスを指定できます。

## 参考リンク
- [SD.exists() - Arduino Reference](https://www.arduino.cc/reference/en/libraries/sd/sdexists/)
- [How to Check if a File Exists Using Arduino](https://maker.pro/arduino/tutorial/how-to-check-if-a-file-exists-using-arduino)
- [Arduino File Input and Output Tutorial](https://www.arduino.cc/en/Tutorial/Files)
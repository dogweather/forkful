---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "Arduino: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

＃＃なぜ 
なぜディレクトリが存在するかどうかを確認する必要があるのか、最大2文で説明します。 

ディレクトリが存在するかどうかを確認することにより、コードの実行中に必要なファイルやデータが存在するかどうかを確認し、その後の処理を制御することができます。また、ディレクトリが存在しない場合は、そのディレクトリを作成することができます。 

＃＃方法 
「```Arduino 
if (SD.exists（"/ directory"））{ 
Serial.println（"ディレクトリが存在します"）; 
}他の{ 
Serial.println（"ディレクトリは存在しません"）; 
}```」 

このコード例では、SD.exists（）関数を使用して、SDカード内の「ディレクトリ」が存在するかどうかを確認しています。もし存在すれば、「ディレクトリが存在します」というメッセージがシリアルモニターに表示されます。存在しない場合は、代わりに「ディレクトリは存在しません」というメッセージが表示されます。 

###深いダイブ 
この例では、SD.exists（）関数を使用してディレクトリが存在するかどうかを確認しましたが、実際にはこの関数を使用してディレクトリだけでなく、ファイルやサブディレクトリの存在も確認することができます。また、ディレクトリが存在しない場合は、SD.mkdir（）関数を使用して新しいディレクトリを作成できます。これにより、プログラムの柔軟性が向上します。詳細な使い方やその他の関数については、Arduinoの公式ウェブサイトやドキュメンテーションを参照してください。 

＃＃参照 
- https://www.arduino.cc/en/Reference/SDexists 
- https://www.arduino.cc/en/Reference/SDmkdir
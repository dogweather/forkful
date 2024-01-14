---
title:                "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# なぜStandard Errorを書くのか

プログラミングをする人々にとって、エラーは日常的なものです。しかし、それらのエラーを解読するのは非常に大変です。そこで、実行中に何が起こっているのかをより正確に把握するために、Standard Errorを書くことが重要です。

# 方法

**Arduinoのライブラリを``#include``することを忘れないように!**
```Arduino
#include <iostream> 
```

**``std::cerr``を使用してStandard Errorに書き込む**
```Arduino
std::cerr << "エラーメッセージ" << std::endl;
```

**サンプル出力:**
```
エラーメッセージ
```

# 深く掘り下げる

Standard Errorに書き込むことで、プログラムの実行中に発生したエラーをターミナルに表示できます。これにより、プログラムをデバッグする際に追加の情報を提供し、問題を追跡するのに役立ちます。ストリーム操作子``<<``を使用して、様々なデータ型の情報をStandard Errorに書き込むことができ、より詳細なデバッグが可能になります。

# 参考文献

- [標準エラー出力 (Standard Error)](https://wa3.i-3-i.info/word11481.html)
- [C++で標準エラー出力](https://qiita.com/kawaMk4/items/bba378e38cb926138d2d)
- [Arduino: Serial.printとSerial.println](https://iot-nozaki.blogspot.com/2015/03/arduino-serialprintserialprintln.html)

# もっと詳しく知りたい方は是非参考にしてください。

## 参考文献

- [標準エラー出力 (Standard Error)](https://wa3.i-3-i.info/word11481.html)
- [C++で標準エラー出力](https://qiita.com/kawaMk4/items/bba378e38cb926138d2d)
- [Arduino: Serial.printとSerial.println](https://iot-nozaki.blogspot.com/2015/03/arduino-serialprintserialprintln.html)
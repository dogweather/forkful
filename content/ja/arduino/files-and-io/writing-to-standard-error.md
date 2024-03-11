---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:42.007025-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:16.065228-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
---

{{< edit_this_page >}}

## 何となぜ？

Arduinoプログラミングにおいて標準エラー(stderr)への書き込みは、エラーメッセージや診断を標準出力(stdout)とは別のチャネルへ向けることを含みます。これにより、エラーメッセージが標準出力と混ざることがないようにし、プログラマーは正常なプログラム出力とエラーメッセージを区別することができます。これにより、デバッグとログ分析がより直接的になります。

## 方法:

Arduinoは、従来のコンピューティングシステムが行うように、標準出力と標準エラーを本来的に区別しません。`Serial.print()`メソッドと`Serial.println()`メソッドの両方が、一般にArduino IDEのシリアルモニタで表示される同じシリアル出力に書き込みます。しかし、エラーメッセージを特別にフォーマットしたり、SDカード上のファイルやネットワーク接続経由で別の出力に向けたりすることで、stderrへ書き込むことをエミュレートすることができます。

stderrをエミュレートするために、エラーメッセージに"ERROR:"のようなタグを前置して、シリアルモニタで区別することができます：

```cpp
void setup() {
  Serial.begin(9600); // 9600のボーレートでシリアル通信を初期化
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // エラーメッセージの前にエミュレートするstderrを前置
    Serial.println("ERROR: 関数の実行に失敗しました。");
  } else {
    Serial.println("関数は正常に実行されました。");
  }
  delay(1000); // ループを再開する前に1秒待つ
}

int someFunction() {
  // エラーの場合-1を返すダミー関数
  return -1;
}
```

Arduino IDEのシリアルモニタでのサンプル出力は、こんな感じになります：

```
ERROR: 関数の実行に失敗しました。
```

異なる物理的出力への書き込みを含む、より洗練されたアプローチが必要なプロジェクトの場合、サードパーティのライブラリや追加のハードウェアが必要になることがあります。例えば、SDカードへのエラーメッセージのログ記録には`SD`ライブラリが必要です：

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("ERROR: SDカードの初期化に失敗しました！");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("ERROR: 関数の実行に失敗しました。");
    myFile.close(); // 内容を保存するため、ファイルを閉じることを確認する
  } else {
    Serial.println("ERROR: error.logのオープンに失敗しました！");
  }
}

void loop() {
  // ここにメインのコードが入ります
}
```

このアプローチにより、SDカード上の`error.log`ファイルへエラーメッセージを向けることで、物理的に正常なプログラム出力とエラーメッセージを分離し、主要な出力チャネルを散らかすことなく事後分析を可能にします。

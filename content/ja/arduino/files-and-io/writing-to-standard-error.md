---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:42.007025-07:00
description: "\u65B9\u6CD5: Arduino\u306F\u3001\u5F93\u6765\u306E\u30B3\u30F3\u30D4\
  \u30E5\u30FC\u30C6\u30A3\u30F3\u30B0\u30B7\u30B9\u30C6\u30E0\u304C\u884C\u3046\u3088\
  \u3046\u306B\u3001\u6A19\u6E96\u51FA\u529B\u3068\u6A19\u6E96\u30A8\u30E9\u30FC\u3092\
  \u672C\u6765\u7684\u306B\u533A\u5225\u3057\u307E\u305B\u3093\u3002`Serial.print()`\u30E1\
  \u30BD\u30C3\u30C9\u3068`Serial.println()`\u30E1\u30BD\u30C3\u30C9\u306E\u4E21\u65B9\
  \u304C\u3001\u4E00\u822C\u306BArduino\u2026"
lastmod: '2024-04-05T22:38:42.025710-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u306F\u3001\u5F93\u6765\u306E\u30B3\u30F3\u30D4\u30E5\u30FC\u30C6\
  \u30A3\u30F3\u30B0\u30B7\u30B9\u30C6\u30E0\u304C\u884C\u3046\u3088\u3046\u306B\u3001\
  \u6A19\u6E96\u51FA\u529B\u3068\u6A19\u6E96\u30A8\u30E9\u30FC\u3092\u672C\u6765\u7684\
  \u306B\u533A\u5225\u3057\u307E\u305B\u3093\u3002`Serial.print()`\u30E1\u30BD\u30C3\
  \u30C9\u3068`Serial.println()`\u30E1\u30BD\u30C3\u30C9\u306E\u4E21\u65B9\u304C\u3001\
  \u4E00\u822C\u306BArduino IDE\u306E\u30B7\u30EA\u30A2\u30EB\u30E2\u30CB\u30BF\u3067\
  \u8868\u793A\u3055\u308C\u308B\u540C\u3058\u30B7\u30EA\u30A2\u30EB\u51FA\u529B\u306B\
  \u66F8\u304D\u8FBC\u307F\u307E\u3059\u3002\u3057\u304B\u3057\u3001\u30A8\u30E9\u30FC\
  \u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u7279\u5225\u306B\u30D5\u30A9\u30FC\u30DE\u30C3\
  \u30C8\u3057\u305F\u308A\u3001SD\u30AB\u30FC\u30C9\u4E0A\u306E\u30D5\u30A1\u30A4\
  \u30EB\u3084\u30CD\u30C3\u30C8\u30EF\u30FC\u30AF\u63A5\u7D9A\u7D4C\u7531\u3067\u5225\
  \u306E\u51FA\u529B\u306B\u5411\u3051\u305F\u308A\u3059\u308B\u3053\u3068\u3067\u3001\
  stderr\u3078\u66F8\u304D\u8FBC\u3080\u3053\u3068\u3092\u30A8\u30DF\u30E5\u30EC\u30FC\
  \u30C8\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

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

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:42.007025-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.517514-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3044\u3066\
  \u6A19\u6E96\u30A8\u30E9\u30FC(stderr)\u3078\u306E\u66F8\u304D\u8FBC\u307F\u306F\
  \u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\u8A3A\u65AD\u3092\u6A19\
  \u6E96\u51FA\u529B(stdout)\u3068\u306F\u5225\u306E\u30C1\u30E3\u30CD\u30EB\u3078\
  \u5411\u3051\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u3053\u308C\u306B\
  \u3088\u308A\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u304C\u6A19\u6E96\
  \u51FA\u529B\u3068\u6DF7\u3056\u308B\u3053\u3068\u304C\u306A\u3044\u3088\u3046\u306B\
  \u3057\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u6B63\u5E38\u306A\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u51FA\u529B\u3068\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\
  \u3092\u533A\u5225\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\
  \u308C\u306B\u3088\u308A\u3001\u30C7\u30D0\u30C3\u30B0\u3068\u30ED\u30B0\u5206\u6790\
  \u304C\u3088\u308A\u76F4\u63A5\u7684\u306B\u306A\u308A\u307E\u3059\u3002."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

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

---
date: 2024-01-20 17:52:03.118530-07:00
description: "How to: \u6DF1\u3044\u60C5\u5831\uFF1A Arduino\u3067\u306F\u3001\u30B7\
  \u30EA\u30A2\u30EB\u30B3\u30DF\u30E5\u30CB\u30B1\u30FC\u30B7\u30E7\u30F3\u3092\u30C7\
  \u30D0\u30C3\u30B0\u306B\u5229\u7528\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\
  Arduino\u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u304C\u767B\u5834\u3057\u3066\
  \u4EE5\u6765\u306E\u4F1D\u7D71\u7684\u306A\u65B9\u6CD5\u3067\u3059\u3002`Serial.print()`\
  \ \u3084 `Serial.println()`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:42.007650-06:00'
model: gpt-4-1106-preview
summary: "\u6DF1\u3044\u60C5\u5831\uFF1A Arduino\u3067\u306F\u3001\u30B7\u30EA\u30A2\
  \u30EB\u30B3\u30DF\u30E5\u30CB\u30B1\u30FC\u30B7\u30E7\u30F3\u3092\u30C7\u30D0\u30C3\
  \u30B0\u306B\u5229\u7528\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001Arduino\u30D7\
  \u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u304C\u767B\u5834\u3057\u3066\u4EE5\u6765\
  \u306E\u4F1D\u7D71\u7684\u306A\u65B9\u6CD5\u3067\u3059\u3002`Serial.print()` \u3084\
  \ `Serial.println()` \u306F\u57FA\u672C\u7684\u306A\u30C7\u30D0\u30C3\u30B0\u624B\
  \u6BB5\u3067\u3001\u30B7\u30EA\u30A2\u30EB\u30DD\u30FC\u30C8\u3092\u7D4C\u7531\u3057\
  \u3066\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u306E\u30B7\u30EA\u30A2\u30EB\u30E2\u30CB\
  \u30BF\u306B\u30C7\u30FC\u30BF\u3092\u9001\u4FE1\u3057\u307E\u3059\u3002\u4ED6\u306E\
  \u9078\u629E\u80A2\u3068\u3057\u3066\u306F\u3001LED\u306E\u70B9\u6EC5\u3084LCD\u30B9\
  \u30AF\u30EA\u30FC\u30F3\u3078\u306E\u51FA\u529B\u304C\u3042\u308A\u307E\u3059\u304C\
  \u3001\u3053\u306E\u624B\u6CD5\u306E\u7C21\u5358\u3055\u3068\u6709\u52B9\u6027\u306F\
  \u975E\u5E38\u306B\u9AD8\u3044\u3067\u3059\u3002`String()` \u95A2\u6570\u3067\u6570\
  \u5B57\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3057\u3001\u8AAD\u307F\u3084\u3059\
  \u3044\u5F62\u3067\u30C7\u30FC\u30BF\u3092\u8868\u793A\u3059\u308B\u3053\u3068\u3082\
  \u53EF\u80FD\u3067\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
weight: 33
---

## How to:
やり方：

```Arduino
void setup() {
  // Start the serial communication
  Serial.begin(9600);
}

void loop() {
  // Print debug output to the serial monitor
  Serial.println("Sensor value: " + String(analogRead(A0)));
  // Wait for a second
  delay(1000);
}
```

サンプル出力:
```
Sensor value: 402
Sensor value: 403
Sensor value: 408
```

## Deep Dive:
深い情報：

Arduinoでは、シリアルコミュニケーションをデバッグに利用します。これは、Arduinoプラットフォームが登場して以来の伝統的な方法です。`Serial.print()` や `Serial.println()` は基本的なデバッグ手段で、シリアルポートを経由してコンピュータのシリアルモニタにデータを送信します。他の選択肢としては、LEDの点滅やLCDスクリーンへの出力がありますが、この手法の簡単さと有効性は非常に高いです。`String()` 関数で数字を文字列に変換し、読みやすい形でデータを表示することも可能です。

実装上の注意点としては、シリアル通信はArduinoボードとPC間でデータをやり取りするため、`Serial.begin()` で通信速度(ボーレート)を設定する必要があります。また、プログラムがリリース版の時は、デバッグ出力用コードを削除するのが一般的です。

## See Also:
関連情報：

- [プログラミングにおけるデバッグの技術](https://www.arduino.cc/en/Tutorial/BuiltInExamples)

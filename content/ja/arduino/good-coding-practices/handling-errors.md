---
date: 2024-01-26 00:50:49.537900-07:00
description: "\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u4E2D\u306B\u4E88\u671F\u305B\u306C\u4E8B\u614B\u304C\u751F\u3058\u3066\u3082\u8DB3\
  \u5143\u3092\u3059\u304F\u308F\u308C\u306A\u3044\u3088\u3046\u306B\u3059\u308B\u305F\
  \u3081\u306E\u3082\u306E\u3067\u3059\u3002\u4E88\u671F\u3057\u306A\u3044\u4E8B\u614B\
  \u304C\u8D77\u3053\u3063\u305F\u969B\u306B\u3001Arduino\u304C\u6A5F\u80FD\u4E0D\u5168\
  \u306B\u9665\u308B\u306E\u3092\u9632\u3050\u305F\u3081\u306B\u884C\u3044\u307E\u3059\
  \u3002"
lastmod: '2024-03-13T22:44:42.507531-06:00'
model: gpt-4-1106-preview
summary: "\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u4E2D\u306B\u4E88\u671F\u305B\u306C\u4E8B\u614B\u304C\u751F\u3058\u3066\u3082\u8DB3\
  \u5143\u3092\u3059\u304F\u308F\u308C\u306A\u3044\u3088\u3046\u306B\u3059\u308B\u305F\
  \u3081\u306E\u3082\u306E\u3067\u3059\u3002\u4E88\u671F\u3057\u306A\u3044\u4E8B\u614B\
  \u304C\u8D77\u3053\u3063\u305F\u969B\u306B\u3001Arduino\u304C\u6A5F\u80FD\u4E0D\u5168\
  \u306B\u9665\u308B\u306E\u3092\u9632\u3050\u305F\u3081\u306B\u884C\u3044\u307E\u3059\
  \u3002."
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

## 方法:
たとえば、Arduinoが時折範囲外の値を出力するセンサーの読み取りを行っているとします。以下のように処理することができます：

```Arduino
int sensorValue = analogRead(A0);

if (sensorValue >= 0 && sensorValue <= 1023) {
  // 値が範囲内にある場合、処理を続行
  Serial.println(sensorValue);
} else {
  // 値が範囲外の場合、エラーを処理
  Serial.println("Error: Sensor value out of range.");
}
```

サンプル出力：
```
523
Error: Sensor value out of range.
761
```

## 詳細解説
エラー処理が今ほど簡単ではなかったこともあります。初期のころ、開発者はよくエラーを無視しており、それが恐れられる「未定義の振る舞い」に繋がっていました。プログラミングが進化するにつれ、ツールも進化しました — 今では多くの言語で例外処理を利用できますが、ハードウェアの制約とC++のルーツのために、Arduinoの世界では未だに「先にチェックする」の古風な手法が用いられています。

Arduinoプログラミングでは、エラー処理のために `if-else` 文をよく見かけます。しかし、代替手段もあります：条件に失敗した場合に実行を停止する `assert` 関数を使う、あるいはハードウェア設定自体に故障対策を設計するなどです。

エラー処理を実装する際は、プログラムを停止することの影響と、デフォルト状態あるいは安全な状態で続行させることを検討してください。トレードオフがあり、正しい選択は中断の潜在的な危害と誤動作のバランスに依存します。

## 参照
エラー検出と処理についての知識を深めるためにこれらを参照してください：

- Arduino言語リファレンス: https://www.arduino.cc/reference/jp/
- Embedded Artistryの分かりやすいエラー処理の解説: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- C++エラー処理: https://en.cppreference.com/w/cpp/error/exception

これで、Arduinoを使った冒険でエラーの罠を避けるための知識と自信が身につきました。

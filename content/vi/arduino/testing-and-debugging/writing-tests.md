---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:53.850550-07:00
description: "C\xE1ch l\xE0m: B\u1ED1i c\u1EA3nh l\u1ECBch s\u1EED: Ki\u1EC3m th\u1EED\
  \ trong Arduino b\u1EAFt \u0111\u1EA7u mu\u1ED9n h\u01A1n trong ph\xE1t tri\u1EC3\
  n ph\u1EA7n m\u1EC1m v\xE0 \xEDt ph\u1ED5 bi\u1EBFn h\u01A1n do t\u01B0\u01A1ng\
  \ t\xE1c v\u1EDBi ph\u1EA7n c\u1EE9ng. C\xE1c ph\u01B0\u01A1ng \xE1n\u2026"
lastmod: '2024-04-05T22:37:45.705171-06:00'
model: gpt-4-0125-preview
summary: "B\u1ED1i c\u1EA3nh l\u1ECBch s\u1EED: Ki\u1EC3m th\u1EED trong Arduino b\u1EAF\
  t \u0111\u1EA7u mu\u1ED9n h\u01A1n trong ph\xE1t tri\u1EC3n ph\u1EA7n m\u1EC1m v\xE0\
  \ \xEDt ph\u1ED5 bi\u1EBFn h\u01A1n do t\u01B0\u01A1ng t\xE1c v\u1EDBi ph\u1EA7\
  n c\u1EE9ng. C\xE1c ph\u01B0\u01A1ng \xE1n thay th\u1EBF: Ki\u1EC3m th\u1EED th\u1EE7\
  \ c\xF4ng, ho\u1EB7c c\xE1c framework ki\u1EC3m th\u1EED ph\u1EE9c t\u1EA1p h\u01A1\
  n nh\u01B0 Google Test. Chi ti\u1EBFt th\u1EF1c hi\u1EC7n: Th\u01B0\u1EDDng ch\xFA\
  ng ta s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7n nh\u01B0 ArduinoUnit ho\u1EB7\
  c AUnit. \u0110\u1EB7t ki\u1EC3m th\u1EED trong `setup()` v\xE0 gi\u1EEF `loop()`\
  \ tr\u1ED1ng v\xEC ki\u1EC3m th\u1EED ch\u1EC9 ch\u1EA1y m\u1ED9t l\u1EA7n."
title: "Vi\u1EBFt ki\u1EC3m th\u1EED"
weight: 36
---

## Cách làm:
```Arduino
#include <Arduino.h>
#include <unity.h>

void setUp(void) {
// thiết lập ở đây
}

void tearDown(void) {
// dọn dẹp ở đây
}

void test_led_builtin_pin_number(void) {
    TEST_ASSERT_EQUAL(13, LED_BUILTIN);
}

void test_led_state_high(void) {
    digitalWrite(LED_BUILTIN, HIGH);
    TEST_ASSERT_EQUAL(digitalRead(LED_BUILTIN), HIGH);
}

void setup() {
    UNITY_BEGIN();
    RUN_TEST(test_led_builtin_pin_number);
    RUN_TEST(test_led_state_high);
    UNITY_END();
}

void loop() {
    // Thường trống trong kiểm thử
}
```
Đầu ra:
```
.
.
OK
```

## Sâu hơn
Bối cảnh lịch sử: Kiểm thử trong Arduino bắt đầu muộn hơn trong phát triển phần mềm và ít phổ biến hơn do tương tác với phần cứng. Các phương án thay thế: Kiểm thử thủ công, hoặc các framework kiểm thử phức tạp hơn như Google Test. Chi tiết thực hiện: Thường chúng ta sử dụng một thư viện như ArduinoUnit hoặc AUnit. Đặt kiểm thử trong `setup()` và giữ `loop()` trống vì kiểm thử chỉ chạy một lần.

## Xem thêm
- Thư viện ArduinoUnit: https://github.com/mmurdoch/arduinounit
- Thư viện AUnit: https://github.com/bxparks/AUnit
- Giới thiệu về Unit Testing: https://www.arduino.cc/en/Guide/UnitTesting

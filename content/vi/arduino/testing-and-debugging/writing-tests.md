---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:53.850550-07:00
description: "C\xE1ch l\xE0m: B\u1ED1i c\u1EA3nh l\u1ECBch s\u1EED: Ki\u1EC3m th\u1EED\
  \ trong Arduino b\u1EAFt \u0111\u1EA7u mu\u1ED9n h\u01A1n trong ph\xE1t tri\u1EC3\
  n ph\u1EA7n m\u1EC1m v\xE0 \xEDt ph\u1ED5 bi\u1EBFn h\u01A1n do t\u01B0\u01A1ng\
  \ t\xE1c v\u1EDBi ph\u1EA7n c\u1EE9ng. C\xE1c ph\u01B0\u01A1ng \xE1n\u2026"
lastmod: '2024-04-05T21:53:38.354605-06:00'
model: gpt-4-0125-preview
summary: ''
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

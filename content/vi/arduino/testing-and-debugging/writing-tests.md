---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:53.850550-07:00
description: "Vi\u1EBFt ki\u1EC3m th\u1EED c\xF3 ngh\u0129a l\xE0 t\u1EA1o ra m\u1ED9\
  t t\u1EADp h\u1EE3p c\xE1c \u0111i\u1EC1u ki\u1EC7n \u0111\u1EC3 ki\u1EC3m tra xem\
  \ m\xE3 c\u1EE7a b\u1EA1n c\xF3 ho\u1EA1t \u0111\u1ED9ng nh\u01B0 mong \u0111\u1EE3\
  i hay kh\xF4ng. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \u2026"
lastmod: '2024-03-13T22:44:36.993544-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt ki\u1EC3m th\u1EED c\xF3 ngh\u0129a l\xE0 t\u1EA1o ra m\u1ED9\
  t t\u1EADp h\u1EE3p c\xE1c \u0111i\u1EC1u ki\u1EC7n \u0111\u1EC3 ki\u1EC3m tra xem\
  \ m\xE3 c\u1EE7a b\u1EA1n c\xF3 ho\u1EA1t \u0111\u1ED9ng nh\u01B0 mong \u0111\u1EE3\
  i hay kh\xF4ng."
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

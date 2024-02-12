---
title:                "Viết kiểm thử"
aliases: - /vi/arduino/writing-tests.md
date:                  2024-01-28T22:12:53.850550-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết kiểm thử"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Viết kiểm thử có nghĩa là tạo ra một tập hợp các điều kiện để kiểm tra xem mã của bạn có hoạt động như mong đợi hay không. Lập trình viên làm điều này để phát hiện lỗi sớm, đảm bảo chất lượng phần mềm và tránh nhức đầu khi thêm tính năng mới.

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

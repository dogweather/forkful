---
title:                "Chuyển đổi chuỗi thành chữ thường"
date:                  2024-01-28T21:58:38.219026-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Chuyển một chuỗi sang chữ thường có nghĩa là biến đổi tất cả các chữ cái trong chuỗi về dạng chữ thường. Lập trình viên thực hiện việc này để chuẩn hóa dữ liệu, đơn giản hóa việc so sánh, và cho mục đích hiển thị thẩm mỹ.

## Cách thực hiện:

Lớp `String` trong Java có phương thức `toLowerCase()` tiện lợi giúp bạn thực hiện công việc khó khăn này. Hãy xem ví dụ đơn giản sau:

```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String banDau = "Java ROCKS!";
        String daChuyenThanhChuThuong = banDau.toLowerCase();
        System.out.println(daChuyenThanhChuThuong);
    }
}
```

Kết quả:

```
java rocks!
```

Đó là tất cả. Chuỗi đã được "hạ âm lượng" xuống chế độ chữ thường dễ chịu.

## Đào Sâu Hơn

Ngày xưa, xử lý văn bản là một công việc rắc rối. Các ngôn ngữ khác nhau, các dạng chữ cái khác nhau, các hệ thống máy tính la hét trong sự nhầm lẫn. Java, ra đời trong những năm '90, đã tìm cách làm mọi thứ dễ dàng hơn. Phương thức `toLowerCase()` đã là một phần của lớp `String` của Java ngay từ những ngày đầu.

Nhưng có một số điều thú vị bên dưới nắp capo. Bạn có thể tự hỏi tại sao `toLowerCase()` lại cần thiết. Điều đó là vì, không phải tất cả các nền văn hóa đều định nghĩa "chữ thường" theo cùng một cách. Phương thức này nhạy cảm với địa phương, sử dụng địa phương mặc định của hệ thống của bạn, hoặc bạn có thể chỉ định một địa phương sử dụng `toLowerCase(Locale locale)`.

Dưới đây là một tình huống khác: các ngôn ngữ với các kịch bản phức tạp hơn, như Tiếng Thổ Nhĩ Kỳ, có các ký tự "i không chấm" đặc biệt có thể làm suy yếu việc chuyển đổi chữ thường thông thường. Vì vậy, Java cung cấp lựa chọn để chuyển đổi ký tự một cách tỉ mỉ.

Có phương án thay thế? Chắc chắn, bạn có thể chạy qua chuỗi bằng vòng lặp `for`, thay đổi ký tự một cách thủ công. Nhưng tại sao phải tái tạo bánh xe khi Java đã hỗ trợ bạn?

Ngoài ra, điều này có thể làm ngạc nhiên cho một số người: chuỗi trong Java là bất biến. Khi bạn sử dụng `toLowerCase()`, bạn không chỉnh sửa chuỗi gốc, bạn đang tạo một chuỗi mới hoàn toàn, áo giáp và tất cả.

## Xem Thêm

Hãy xem qua các tài nguyên này để nâng cao trò chơi chuỗi của bạn:

- Java String API: [](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- Java Locale Class: [](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Locale.html)
- Unicode Case Mappings: [](https://unicode.org/reports/tr21/)

Và để biết chi tiết rõ ràng về Tiêu chuẩn Unicode:

- The Unicode Consortium: [](https://unicode.org/)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:38.219026-07:00
description: "Chuy\u1EC3n m\u1ED9t chu\u1ED7i sang ch\u1EEF th\u01B0\u1EDDng c\xF3\
  \ ngh\u0129a l\xE0 bi\u1EBFn \u0111\u1ED5i t\u1EA5t c\u1EA3 c\xE1c ch\u1EEF c\xE1\
  i trong chu\u1ED7i v\u1EC1 d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 chu\u1EA9n h\xF3a d\u1EEF\
  \u2026"
lastmod: '2024-03-13T22:44:36.472125-06:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n m\u1ED9t chu\u1ED7i sang ch\u1EEF th\u01B0\u1EDDng c\xF3 ngh\u0129\
  a l\xE0 bi\u1EBFn \u0111\u1ED5i t\u1EA5t c\u1EA3 c\xE1c ch\u1EEF c\xE1i trong chu\u1ED7\
  i v\u1EC1 d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

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

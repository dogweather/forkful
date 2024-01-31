---
title:                "Làm tròn số"
date:                  2024-01-28T22:06:53.206409-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm tròn số"

category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Điều gì & Tại sao?
Làm tròn số có nghĩa là điều chỉnh chúng để đạt một độ chính xác chỉ định. Lập trình viên thực hiện việc này để đơn giản hóa số liệu cho dễ đọc, để đáp ứng các thông số kỹ thuật nhất định, hoặc để đảm bảo các phép tính nằm trong các giới hạn nhất định, như tránh lỗi chính xác trong tính toán số thực dấu phẩy động.

## Cách thực hiện:
Java cung cấp nhiều cách để làm tròn số. Dưới đây là một ví dụ nhanh với `Math.round()`, `BigDecimal`, và `DecimalFormat`.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Sử dụng Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // Kết quả: 123

        // Sử dụng BigDecimal để có thêm kiểm soát
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // Kết quả: 123.46

        // Sử dụng DecimalFormat
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // Kết quả: 123.46
    }
}
```

## Nghiên cứu sâu
Lịch sử, việc làm tròn số đã rất cần thiết cho các phép tính analog và đã được kế thừa trong tính toán kỹ thuật số vì hiệu quả và độ chính xác. Lỗi làm tròn, như những lỗi từ tính toán số thực dấu phẩy động, chứng minh rằng đây không phải là vấn đề nhỏ - chúng có thể cộng dồn làm hỏng các phép tính trong, ví dụ, ứng dụng hàng không và tài chính.

Ngoài `Math.round()`, bạn có `BigDecimal`, nó mang lại cho bạn sự kiểm soát tốt hơn về tỉ lệ và chế độ làm tròn, và `DecimalFormat` cho khi bạn cần làm tròn số như một phần của định dạng đầu ra văn bản. Các phương án thay thế cho việc làm tròn bao gồm làm tròn xuống, làm tròn lên, và cắt bỏ, đây là những cách khác nhau để xử lý độ chính xác và thường được xử lý bởi các phương thức `Math` khác nhau.

Tùy theo trường hợp sử dụng của bạn, chiến lược làm tròn có thể thay đổi. Ví dụ, `BigDecimal` là lựa chọn hàng đầu cho các phép tính tài chính, nơi mà độ chính xác rất quan trọng. Ngược lại, `Math.round()` là một cách nhanh chóng cho các hoạt động mục đích chung khi bạn ít cầu kỳ về chế độ làm tròn.

## Xem thêm
- [Tài liệu Java Math của Oracle](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [Tiêu chuẩn IEEE cho Số học Dấu phẩy động (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [Lớp DecimalFormat trong Java](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:53.206409-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Java cung c\u1EA5p nhi\u1EC1u c\xE1ch \u0111\
  \u1EC3 l\xE0m tr\xF2n s\u1ED1. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5\
  \ nhanh v\u1EDBi `Math.round()`, `BigDecimal`, v\xE0 `DecimalFormat`."
lastmod: '2024-03-13T22:44:36.482468-06:00'
model: gpt-4-0125-preview
summary: "Java cung c\u1EA5p nhi\u1EC1u c\xE1ch \u0111\u1EC3 l\xE0m tr\xF2n s\u1ED1\
  ."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:28.659989-07:00
description: "S\u1ED1 ph\u1EE9c m\u1EDF r\u1ED9ng d\xE3y s\u1ED1 th\u1EF1c th\xF4\
  ng qua vi\u1EC7c th\xEAm m\u1ED9t \u0111\u01A1n v\u1ECB \u1EA3o, `i`, n\u01A1i m\xE0\
  \ `i^2 = -1`. Ch\xFAng r\u1EA5t quan tr\u1ECDng trong c\xE1c l\u0129nh v\u1EF1c\
  \ nh\u01B0 k\u1EF9 thu\u1EADt, v\u1EADt l\xFD v\xE0\u2026"
lastmod: 2024-02-19 22:04:55.643237
model: gpt-4-0125-preview
summary: "S\u1ED1 ph\u1EE9c m\u1EDF r\u1ED9ng d\xE3y s\u1ED1 th\u1EF1c th\xF4ng qua\
  \ vi\u1EC7c th\xEAm m\u1ED9t \u0111\u01A1n v\u1ECB \u1EA3o, `i`, n\u01A1i m\xE0\
  \ `i^2 = -1`. Ch\xFAng r\u1EA5t quan tr\u1ECDng trong c\xE1c l\u0129nh v\u1EF1c\
  \ nh\u01B0 k\u1EF9 thu\u1EADt, v\u1EADt l\xFD v\xE0\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Số phức mở rộng dãy số thực thông qua việc thêm một đơn vị ảo, `i`, nơi mà `i^2 = -1`. Chúng rất quan trọng trong các lĩnh vực như kỹ thuật, vật lý và toán học nâng cao, nơi chúng mô hình hóa các hiện tượng mà số thực không thể xử lý, như dòng điện và xử lý tín hiệu.

## Làm thế nào:

Java không có hỗ trợ sẵn cho số phức, nhưng chúng ta có thể tự tạo lớp hoặc sử dụng một thư viện. Dưới đây là một ví dụ nhanh về cách tạo một lớp `ComplexNumber` đơn giản và sử dụng nó:

```java
public class ComplexNumber {
    private double real;
    private double imaginary;

    public ComplexNumber(double real, double imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
    }

    // ToString để hiển thị số phức dưới dạng a + bi
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // Kiểm tra nhanh
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Tổng: " + c1.add(c2));
    }
}
```

Đầu ra mẫu cho phương thức main sẽ là:

```
Tổng: 3.0 + 7.0i
```

## Sâu hơn

Trước khi có các ngôn ngữ cấp cao như Java, lập trình viên làm việc trực tiếp với các thư viện toán học trong các ngôn ngữ như Fortran hoặc C để quản lý các phép toán phức tạp. Khái niệm trở về thế kỷ 16, được ghi nhận do các nhà toán học như Gerolamo Cardano và Rafael Bombelli.

Trong Java, `java.lang.Math` là điểm đến cho các nhu cầu cơ bản nhưng bỏ qua số phức, có lẽ bởi vì không phải lập trình viên nào cũng sử dụng chúng. Các lựa chọn khác? Sử dụng thư viện. Apache Commons Math cung cấp một lớp `Complex` chứa đầy các phương pháp để thao tác. Đây là lý do tại sao tự tạo của bạn là tốt: Nhẹ, được tùy chỉnh theo nhu cầu cụ thể của bạn, và không có overhead của thư viện.

Một chi tiết quan trọng: cẩn thận với độ chính xác của số điểm dấu phẩy động. Máy tính không thể biểu diễn một số số một cách chính xác, dẫn đến lỗi làm tròn. Khi thực hiện các phép toán phức tạp lặp đi lặp lại, những lỗi này có thể tích tụ!

## Xem thêm

Để khám phá sâu hơn và thực hiện các phép toán phức tạp hơn, kiểm tra:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [Lớp Complex của JScience](http://jscience.org/)
- Các hướng dẫn của Oracle về [toán học điểm dấu phẩy động](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)

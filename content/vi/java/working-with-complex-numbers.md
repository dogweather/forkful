---
title:                "Làm việc với số phức"
date:                  2024-01-28T22:13:28.659989-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

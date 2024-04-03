---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:09.838901-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y th\u1EA3o lu\u1EADn m\u1ED9t s\u1ED1\
  \ m\xE3 C# s\u1EED d\u1EE5ng NUnit, m\u1ED9t framework test ph\u1ED5 bi\u1EBFn:\
  \ 1. Thi\u1EBFt l\u1EADp framework test c\u1EE7a b\u1EA1n - th\u01B0\u1EDDng \u0111\
  \u01B0\u1EE3c bao g\u1ED3m nh\u01B0 m\u1ED9t g\xF3i\u2026"
lastmod: '2024-03-13T22:44:36.662313-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y th\u1EA3o lu\u1EADn m\u1ED9t s\u1ED1 m\xE3 C# s\u1EED d\u1EE5ng NUnit,\
  \ m\u1ED9t framework test ph\u1ED5 bi\u1EBFn."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

## Làm thế nào:
Hãy thảo luận một số mã C# sử dụng NUnit, một framework test phổ biến:

1. Thiết lập framework test của bạn - thường được bao gồm như một gói NuGet.
2. Viết một test cho một hàm đơn giản.

Dưới đây là một ví dụ nhanh về test cho phương thức `Sum`:

```C#
using NUnit.Framework;

namespace CalculatorTests {
    public class Calculator {
        public int Sum(int a, int b) {
            return a + b;
        }
    }

    [TestFixture]
    public class CalculatorTests {
        [Test]
        public void TestSum() {
            var calculator = new Calculator();
            var result = calculator.Sum(2, 3);
            Assert.AreEqual(5, result);
        }
    }
}
```

Chạy test. Nếu nó qua, bạn sẽ thấy:

```
Test Passed
```

Nếu không, bạn sẽ nhận được chi tiết về lý do nó thất bại.

## Tìm hiểu sâu
Unit testing đã phát triển kể từ những năm 1970. Những tiến bộ đáng chú ý bao gồm phát triển dựa trên test và các framework test tự động. Đối với C#, MSTest và xUnit là các lựa chọn vững chắc thay thế NUnit. Các điểm chính bao gồm:

1. **Bối cảnh Lịch sử**: Kent Beck, cùng với những người khác, đã phát triển kiến trúc xUnit làm nền tảng cho nhiều framework.
2. **Các Lựa Chọn Thay Thế**: MSTest là framework test bản địa của Microsoft, trong khi xUnit là một công cụ miễn phí, mã nguồn mở.
3. **Chi Tiết Triển Khai**: Các test nên được cô lập, có thể lặp lại, và nhanh chóng. Chạy chúng như một phần của quy trình xây dựng sản phẩm của bạn.

## Xem Thêm
- [Tài liệu NUnit](https://docs.nunit.org/)
- [Tổng quan về Kiểm thử của Microsoft](https://docs.microsoft.com/en-us/dotnet/core/testing/)
- [GitHub xUnit](https://github.com/xunit/xunit)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:09.838901-07:00
description: "Vi\u1EBFt test trong l\u1EADp tr\xECnh ngh\u0129a l\xE0 t\u1EA1o ra\
  \ c\xE1c mini-ch\u01B0\u01A1ng tr\xECnh \u0111\u1EC3 ki\u1EC3m tra xem m\xE3 c\u1EE7\
  a b\u1EA1n c\xF3 ho\u1EA1t \u0111\u1ED9ng nh\u01B0 mong \u0111\u1EE3i hay kh\xF4\
  ng. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u\u2026"
lastmod: '2024-02-25T18:49:35.005143-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt test trong l\u1EADp tr\xECnh ngh\u0129a l\xE0 t\u1EA1o ra c\xE1\
  c mini-ch\u01B0\u01A1ng tr\xECnh \u0111\u1EC3 ki\u1EC3m tra xem m\xE3 c\u1EE7a b\u1EA1\
  n c\xF3 ho\u1EA1t \u0111\u1ED9ng nh\u01B0 mong \u0111\u1EE3i hay kh\xF4ng. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u\u2026"
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Viết test trong lập trình nghĩa là tạo ra các mini-chương trình để kiểm tra xem mã của bạn có hoạt động như mong đợi hay không. Lập trình viên làm điều này để bắt lỗi, đảm bảo chất lượng, và tiết kiệm thời gian sửa chữa vấn đề sau này.

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

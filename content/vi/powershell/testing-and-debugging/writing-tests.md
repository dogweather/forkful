---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:14:01.965126-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9\
  t b\xE0i test nhanh cho h\xE0m c\u1ED9ng s\u1ED1 s\u1EED d\u1EE5ng Pester, b\u1ED9\
  \ khung ki\u1EC3m th\u1EED c\u1EE7a PowerShell. B\u1EA1n th\u01B0\u1EDDng l\u01B0\
  u k\u1ECBch b\u1EA3n n\xE0y v\u1EDBi t\xEAn\u2026"
lastmod: '2024-03-13T22:44:36.941184-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t b\xE0i test nhanh cho h\xE0m c\u1ED9\
  ng s\u1ED1 s\u1EED d\u1EE5ng Pester, b\u1ED9 khung ki\u1EC3m th\u1EED c\u1EE7a PowerShell."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

## Cách thực hiện:
Dưới đây là một bài test nhanh cho hàm cộng số sử dụng Pester, bộ khung kiểm thử của PowerShell. Bạn thường lưu kịch bản này với tên `Add.Tests.ps1`.

```PowerShell
# Hàm mẫu để kiểm thử
function Add ($a, $b) {
    return $a + $b
}

# Nhập mô-đun Pester
Import-Module Pester

# Định nghĩa bài kiểm thử
Describe "Add-Function" {
    It "cộng hai số" {
        # Sắp xếp
        $num1 = 10
        $num2 = 20
        $expected = 30

        # Thực hiện
        $result = Add $num1 $num2

        # Kiểm tra
        $result | Should -Be $expected
    }
}

# Chạy bài kiểm thử
Invoke-Pester
```

Sau khi chạy kịch bản, bạn sẽ thấy kết quả như:

```
Describing Add-Function
    [+] cộng hai số 75ms
Các bài kiểm thử hoàn thành trong 75ms
Kiểm thử Đã Pass: 1, Fail: 0, Bỏ qua: 0 Không Chạy: 0
```

## Sâu hơn:
Trước đây, việc kiểm thử trong PowerShell thường cần nhiều thao tác thủ công trước khi Pester được giới thiệu. Nó đã thay đổi cuộc chơi bằng cách cung cấp một cú pháp mạnh mẽ nhưng đơn giản cho kiểm thử tự động, mượn khái niệm từ các bộ khung kiểm thử trong các ngôn ngữ khác. Các lựa chọn khác thay cho Pester bao gồm PSUnit và PSTest, nhưng Pester là phương pháp được sử dụng rộng rãi nhất và được tích hợp trực tiếp vào PowerShell Core để hỗ trợ đa nền tảng. Việc triển khai thử nghiệm chi tiết bao gồm một chu trình thường được gọi là "Đỏ, Xanh, Tái cấu trúc", nơi các bài kiểm thử được viết để không pass từ đầu (Đỏ), sau đó mã được viết để pass các bài kiểm thử (Xanh), tiếp theo là giai đoạn làm sạch mà không thay đổi hành vi (Tái cấu trúc).

## Xem thêm:
- Kho GitHub của Pester: [https://github.com/pester/Pester](https://github.com/pester/Pester)

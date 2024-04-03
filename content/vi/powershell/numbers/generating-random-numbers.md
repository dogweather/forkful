---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:44.137420-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: PowerShell cung c\u1EA5p m\u1ED9t c\xE1\
  ch th\u1EB3ng th\u1EAFn \u0111\u1EC3 t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn b\u1EB1\
  ng c\xE1ch s\u1EED d\u1EE5ng cmdlet `Get-Random`. Cmdlet n\xE0y c\xF3 th\u1EC3 s\u1EA3\
  n xu\u1EA5t ra s\u1ED1 ng\u1EABu\u2026"
lastmod: '2024-03-13T22:44:36.928011-06:00'
model: gpt-4-0125-preview
summary: "PowerShell cung c\u1EA5p m\u1ED9t c\xE1ch th\u1EB3ng th\u1EAFn \u0111\u1EC3\
  \ t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng cmdlet\
  \ `Get-Random`."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Cách thực hiện:
PowerShell cung cấp một cách thẳng thắn để tạo số ngẫu nhiên bằng cách sử dụng cmdlet `Get-Random`. Cmdlet này có thể sản xuất ra số ngẫu nhiên trong phạm vi mặc định hoặc một phạm vi được chỉ định.

```PowerShell
# Tạo một số ngẫu nhiên giữa 0 và Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

Để chỉ định một phạm vi, sử dụng các tham số `-Minimum` và `-Maximum`:

```PowerShell
# Tạo một số ngẫu nhiên giữa 1 và 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

Để kiểm soát nhiều hơn, bạn có thể tạo một đối tượng của lớp `System.Random`:

```PowerShell
# Sử dụng System.Random cho một chuỗi các số
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

Nếu bạn cần một lựa chọn ngẫu nhiên từ một mảng hoặc bộ sưu tập, `Get-Random` có thể trực tiếp chọn một mục:

```PowerShell
# Lựa chọn ngẫu nhiên từ một mảng
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## Sâu hơn
Cmdlet `Get-Random` trong PowerShell sử dụng lớp .NET `System.Random` bên dưới để tạo ra các số giả ngẫu nhiên. Chúng được gọi là "giả" vì chúng sử dụng các thuật toán để sản xuất các chuỗi số chỉ trông như ngẫu nhiên. Đối với hầu hết các ứng dụng, mức độ ngẫu nhiên này là đủ. Tuy nhiên, đối với các trường hợp sử dụng cần đến bảo mật mật mã học, `System.Random` không phù hợp do bản chất có thể dự đoán của nó.

PowerShell và .NET cung cấp `System.Security.Cryptography.RNGCryptoServiceProvider` cho tính ngẫu nhiên mật mã học, phù hợp hơn cho việc tạo khóa mã hóa hoặc các hoạt động nhạy cảm với bảo mật khác:

```PowerShell
# Số ngẫu nhiên bảo mật mật mã học
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

Trong khi `Get-Random` và `System.Random` đáp ứng một tập hợp rộng lớn các nhu cầu về ngẫu nhiên trong viết script và lô-gíc ứng dụng, việc chọn công cụ đúng cho công việc là thiết yếu, đặc biệt là trong các ứng dụng tập trung vào bảo mật nơi sự dự đoán có thể là một lỗ hổng.

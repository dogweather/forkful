---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:12.757633-07:00
description: "L\xE0m th\u1EBF n\xE0o: PowerShell kh\xF4ng c\xF3 c\xF4ng c\u1EE5 refactoring\
  \ chuy\xEAn d\u1EE5ng t\xEDch h\u1EE3p s\u1EB5n, nh\u01B0ng b\u1EA1n v\u1EABn c\xF3\
  \ th\u1EC3 d\u1ECDn d\u1EB9p m\xE3 c\u1EE7a m\xECnh cho t\xEDnh d\u1EC5 \u0111\u1ECD\
  c v\xE0 hi\u1EC7u su\u1EA5t. Xem\u2026"
lastmod: '2024-03-13T22:44:36.947538-06:00'
model: gpt-4-0125-preview
summary: "PowerShell kh\xF4ng c\xF3 c\xF4ng c\u1EE5 refactoring chuy\xEAn d\u1EE5\
  ng t\xEDch h\u1EE3p s\u1EB5n, nh\u01B0ng b\u1EA1n v\u1EABn c\xF3 th\u1EC3 d\u1ECD\
  n d\u1EB9p m\xE3 c\u1EE7a m\xECnh cho t\xEDnh d\u1EC5 \u0111\u1ECDc v\xE0 hi\u1EC7\
  u su\u1EA5t."
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
weight: 19
---

## Làm thế nào:
PowerShell không có công cụ refactoring chuyên dụng tích hợp sẵn, nhưng bạn vẫn có thể dọn dẹp mã của mình cho tính dễ đọc và hiệu suất. Xem xét một hàm đang làm quá nhiều việc và làm thế nào chúng ta có thể refactor nó cho sự rõ ràng:

```PowerShell
function Get-InventoryData {
    # Hàm gốc kết hợp việc lấy dữ liệu và định dạng
    $data = Get-Content -Path 'C:\inventory-list.txt'
    $inventoryData = $data | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
    $inventoryData | Format-Table -AutoSize
}

# Được refactor thành các hàm riêng biệt
function Import-InventoryData {
    param($Path)
    Get-Content -Path $Path | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
}

function Format-InventoryData {
    param($Data)
    $Data | Format-Table -AutoSize
}

# Cách sử dụng
$inventory = Import-InventoryData -Path 'C:\inventory-list.txt'
Format-InventoryData -Data $inventory
```

Kết quả mẫu:

```
ItemID Name            Count Price
------ ----            ----- -----
1001   Widget Type A   50    9.99
1002   Gadget Type B   20    14.99
```

## Sâu hơn nữa
Refactoring trong lập trình có nguồn gốc kéo dài trở lại những ngày đầu của phát triển phần mềm, mặc dù nó đã được chính thức hóa là một thực hành vào những năm 1990. Cuốn sách của Martin Fowler "Refactoring: Improving the Design of Existing Code" là một trong những công trình tiên phong về chủ đề này, nhấn mạnh tầm quan trọng của refactoring trong việc đạt được mã sạch.

Mặc dù PowerShell không đi kèm với công cụ refactoring cụ thể như một số Môi trường Phát triển Tích hợp (IDEs) cho các ngôn ngữ khác làm (như Eclipse hay Visual Studio), bạn vẫn có thể thực hành các nguyên tắc refactoring tốt một cách thủ công. Điều quan trọng cần nhớ là refactoring không chỉ là việc thay đổi mã cho việc thay đổi của nó, mà là việc thực hiện các thay đổi có chủ ý, giữ nguyên hành vi mà cải thiện cấu trúc và thiết kế của mã.

Các lựa chọn thay thế cho refactoring thủ công trong PowerShell bao gồm sử dụng các IDE hỗ trợ ngôn ngữ, như Visual Studio Code với tiện ích mở rộng PowerShell, cung cấp các tính năng như định dạng mã và khả năng refactoring cơ bản. Đối với việc refactoring quan trọng hơn, bạn có thể xem xét tận dụng các bài kiểm tra Pester để đảm bảo rằng các thay đổi không làm thay đổi chức năng.

Ngoài ra, việc thực hiện refactoring có thể liên quan đến các thay đổi hệ thống hơn như modular hóa, nơi mã được chia thành các mô-đun hoặc hàm có thể tái sử dụng, cải thiện việc tuân thủ nguyên tắc DRY (Don't Repeat Yourself). Các kỹ thuật refactoring phổ biến khác bao gồm đổi tên cho sự rõ ràng, loại bỏ mã trùng lặp, và giảm thiểu độ phức tạp của logic điều kiện.

## Xem Thêm
Để tìm hiểu sâu hơn, dưới đây là một số nguồn lực:

- Sách Refactoring của Martin Fowler: [_Refactoring: Improving the Design of Existing Code_](https://martinfowler.com/books/refactoring.html)
- Kiểm tra mã đã được refactor với Pester: [Khung Kiểm tra Pester](https://pester.dev/)
- PowerShell Best Practices: [Hướng dẫn Thực hành và Phong cách tốt nhất của PowerShell](https://poshcode.gitbooks.io/powershell-practice-and-style/)

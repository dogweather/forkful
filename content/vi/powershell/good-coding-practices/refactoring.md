---
aliases:
- /vi/powershell/refactoring/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:12.757633-07:00
description: "Refactoring l\xE0 qu\xE1 tr\xECnh c\u1EA5u tr\xFAc l\u1EA1i m\xE3 m\xE1\
  y t\xEDnh hi\u1EC7n c\xF3 m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh vi b\xEAn ngo\xE0\
  i c\u1EE7a n\xF3, nh\u1EB1m m\u1EE5c \u0111\xEDch c\u1EA3i thi\u1EC7n c\xE1c thu\u1ED9\
  c t\xEDnh kh\xF4ng ch\u1EE9c\u2026"
lastmod: 2024-02-18 23:08:50.943487
model: gpt-4-0125-preview
summary: "Refactoring l\xE0 qu\xE1 tr\xECnh c\u1EA5u tr\xFAc l\u1EA1i m\xE3 m\xE1\
  y t\xEDnh hi\u1EC7n c\xF3 m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh vi b\xEAn ngo\xE0\
  i c\u1EE7a n\xF3, nh\u1EB1m m\u1EE5c \u0111\xEDch c\u1EA3i thi\u1EC7n c\xE1c thu\u1ED9\
  c t\xEDnh kh\xF4ng ch\u1EE9c\u2026"
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Refactoring là quá trình cấu trúc lại mã máy tính hiện có mà không thay đổi hành vi bên ngoài của nó, nhằm mục đích cải thiện các thuộc tính không chức năng của phần mềm. Các lập trình viên refactor mã để làm cho nó sạch hơn, hiệu quả hơn và dễ hiểu hơn, giúp việc bảo trì và nâng cấp trong tương lai dễ dàng hơn.

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

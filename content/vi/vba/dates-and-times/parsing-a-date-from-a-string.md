---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:35.875869-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: VBA cung c\u1EA5p m\u1ED9t c\xE1ch tr\u1EF1\
  c ti\u1EBFp \u0111\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p m\u1ED9t chu\u1ED7i th\xE0\
  nh ng\xE0y b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng h\xE0m `CDate` ho\u1EB7c h\xE0m `DateValue`.\
  \ Tuy nhi\xEAn,\u2026"
lastmod: '2024-03-13T22:44:36.445508-06:00'
model: gpt-4-0125-preview
summary: "VBA cung c\u1EA5p m\u1ED9t c\xE1ch tr\u1EF1c ti\u1EBFp \u0111\u1EC3 ph\xE2\
  n t\xEDch c\xFA ph\xE1p m\u1ED9t chu\u1ED7i th\xE0nh ng\xE0y b\u1EB1ng c\xE1ch s\u1EED\
  \ d\u1EE5ng h\xE0m `CDate` ho\u1EB7c h\xE0m `DateValue`."
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xFD t\u1EF1"
weight: 30
---

## Cách thực hiện:
VBA cung cấp một cách trực tiếp để phân tích cú pháp một chuỗi thành ngày bằng cách sử dụng hàm `CDate` hoặc hàm `DateValue`. Tuy nhiên, điều quan trọng là chuỗi phải ở trong một định dạng ngày đúng.

Dưới đây là một ví dụ cơ bản sử dụng `CDate`:

```basic
Sub ParseDateUsingCDate()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "2023-04-01"
    parsedDate = CDate(dateString)
    
    Debug.Print "Ngày đã phân tích cú pháp: "; parsedDate
End Sub
```

Nếu bạn chạy đoạn mã này, đầu ra trong Cửa sổ Ngay lập tức (truy cập qua `Ctrl+G` trong trình soạn thảo VBA) sẽ là:

```
Ngày đã phân tích cú pháp: 4/1/2023 
```

Ngoài ra, bạn có thể sử dụng hàm `DateValue`, hay chính xác hơn về ngày (bỏ qua phần thời gian):

```basic
Sub ParseDateUsingDateValue()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "Ngày 1 Tháng 4, 2023"
    parsedDate = DateValue(dateString)
    
    Debug.Print "Ngày đã phân tích cú pháp bằng DateValue: "; parsedDate
End Sub
```

Mẫu đầu ra cho này cũng sẽ hiển thị trong Cửa sổ Ngay lập tức:

```
Ngày đã phân tích cú pháp bằng DateValue: 4/1/2023
```

Hãy nhớ rằng, sự thành công của việc phân tích cú pháp phụ thuộc vào việc định dạng ngày của chuỗi khớp với cài đặt hệ thống hoặc ứng dụng.

## Kỹ lưỡng hơn
Bên trong, khi VBA phân tích cú pháp một chuỗi thành ngày, nó sử dụng các cài đặt khu vực của hệ điều hành Windows để giải thích định dạng ngày. Điều này rất quan trọng để hiểu vì một chuỗi ngày được phân tích cú pháp hoàn hảo trên một hệ thống có thể gây ra lỗi trên hệ thống khác nếu chúng sử dụng các cài đặt ngày/giờ khác nhau.

Lịch sử, xử lý ngày đã là một nguồn cơn của các lỗi trong ứng dụng, đặc biệt là những ứng dụng được sử dụng quốc tế. Sự phụ thuộc vào các cài đặt khu vực trong VBA là lý do mà một số người có thể xem xét các alternativ như định dạng ISO 8601 (ví dụ: "YYYY-MM-DD") để biểu diễn và phân tích cú pháp ngày một cách không mơ hồ trên các hệ thống khác nhau. Thật không may, VBA không hỗ trợ ISO 8601 một cách tự nhiên và việc phân tích cú pháp thủ công sẽ được cần cho tuân thủ chặt chẽ.

Đối với việc phân tích cú pháp ngày phức tạp hơn ngoài những gì `CDate` hoặc `DateValue` có thể xử lý, hoặc để đảm bảo phân tích cú pháp nhất quán bất kể cài đặt địa phương của hệ thống, các lập trình viên có thể recour đến các hàm phân tích cú pháp tùy chỉnh. Những cái này có thể bao gồm việc tách chuỗi ngày thành các thành phần (năm, tháng, ngày) và xây dựng một ngày sử dụng hàm `DateSerial`. Người khác có thể chọn ngôn ngữ hoặc thư viện mạnh mẽ hơn được thiết kế với ý định quốc tế hoá cho những tác vụ này.

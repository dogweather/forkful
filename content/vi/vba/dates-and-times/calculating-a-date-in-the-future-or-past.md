---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:31.448356-07:00
description: "Vi\u1EC7c t\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai\
  \ ho\u1EB7c qu\xE1 kh\u1EE9 li\xEAn quan \u0111\u1EBFn vi\u1EC7c x\xE1c \u0111\u1ECB\
  nh m\u1ED9t ng\xE0y c\xE1ch m\u1ED9t ng\xE0y cho tr\u01B0\u1EDBc m\u1ED9t s\u1ED1\
  \ ng\xE0y, th\xE1ng, ho\u1EB7c n\u0103m nh\u1EA5t \u0111\u1ECBnh.\u2026"
lastmod: '2024-03-13T22:44:36.450872-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7\
  c qu\xE1 kh\u1EE9 li\xEAn quan \u0111\u1EBFn vi\u1EC7c x\xE1c \u0111\u1ECBnh m\u1ED9\
  t ng\xE0y c\xE1ch m\u1ED9t ng\xE0y cho tr\u01B0\u1EDBc m\u1ED9t s\u1ED1 ng\xE0y,\
  \ th\xE1ng, ho\u1EB7c n\u0103m nh\u1EA5t \u0111\u1ECBnh.\u2026"
title: "T\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1\
  \ kh\u1EE9"
weight: 26
---

## Cái gì & Tại sao?
Việc tính toán một ngày trong tương lai hoặc quá khứ liên quan đến việc xác định một ngày cách một ngày cho trước một số ngày, tháng, hoặc năm nhất định. Các lập trình viên thường cần chức năng này để tự động hóa các công việc như nhắc nhở, đăng ký, ngày hết hạn, và lên lịch các công việc trong các ứng dụng khác nhau.

## Làm thế nào:
Trong Visual Basic for Applications (VBA), hàm chính được sử dụng để tính toán ngày trong tương lai hoặc quá khứ là `DateAdd()`. Hàm này thêm một khoảng thời gian cụ thể vào một ngày, trả về một ngày mới.

Đây là một ví dụ cơ bản để thêm 10 ngày vào ngày hiện tại:

```vb
Dim futureDate As Date
futureDate = DateAdd("d", 10, Date) ' Thêm 10 ngày vào ngày hiện tại
Debug.Print futureDate ' Xuất ra một cái gì đó như: 04/20/2023
```

Tương tự, để tìm một ngày 10 ngày trong quá khứ:

```vb
Dim pastDate As Date
pastDate = DateAdd("d", -10, Date) ' Trừ đi 10 ngày từ ngày hiện tại
Debug.Print pastDate ' Xuất ra: 03/31/2023, giả sử hôm nay là 04/10/2023
```

Những ví dụ này khá dễ hiểu. Bạn có thể thay thế `"d"` bằng các mã khoảng thời gian khác, chẳng hạn như `"m"` cho tháng và `"yyyy"` cho năm, để tính toán các loại ngày khác nhau. Dưới đây là cách bạn có thể tính toán một ngày một năm trong tương lai:

```vb
Dim nextYear As Date
nextYear = DateAdd("yyyy", 1, Date) ' Thêm 1 năm vào ngày hiện tại
Debug.Print nextYear ' Xuất ra: 04/10/2024 nếu hôm nay là 04/10/2023
```

## Đi sâu hơn
Hàm `DateAdd` đã là một phần cơ bản của VBA kể từ khi nó ra đời, được phát triển từ người tiền nhiệm BASIC. Mặc dù nó cung cấp sự đơn giản trong việc thêm hoặc trừ khoảng thời gian từ các ngày, điều quan trọng cần lưu ý là VBA, bao gồm các hàm xử lý ngày của nó, có thể không luôn ưu việt về sự tiện lợi hoặc hiệu quả so với các ngôn ngữ lập trình mới hơn.

Chẳng hạn, các ngôn ngữ hiện đại như Python với mô-đun `datetime` hoặc JavaScript với các thư viện như `moment.js` và `date-fns` cung cấp các cách thức linh hoạt và mạnh mẽ hơn cho việc điều chỉnh ngày. Những lựa chọn này cung cấp sự hỗ trợ tốt hơn cho việc địa phương hóa, múi giờ, và năm nhuận, có thể làm cho chúng phù hợp hơn cho các ứng dụng cần tính toán ngày chính xác trên quy mô toàn cầu.

Tuy nhiên, đối với các macro Excel và ứng dụng yêu cầu tích hợp trong hệ sinh thái Microsoft Office, VBA vẫn là một lựa chọn thực tiễn. Sự đơn giản trong việc truy cập và thao tác dữ liệu Excel là một lợi ích đáng kể. Hơn nữa, đối với hầu hết các tính toán ngày cơ bản như lên lịch và nhắc nhở, `DateAdd()` trong VBA cung cấp một giải pháp đủ và dễ hiểu. Cú pháp của nó dễ nắm bắt đối với người mới, trong khi sự tích hợp của nó vào các ứng dụng bộ Office đảm bảo tính liên quan trong các trường hợp sử dụng cụ thể.

Kết luận, trong khi các ngôn ngữ lập trình khác có thể cung cấp những cách tiếp cận hiện đại hơn cho việc tính toán ngày, `DateAdd()` trong VBA chứng minh sức mạnh và vị thế của ngôn ngữ trong những lĩnh vực nó cần thiết nhất.

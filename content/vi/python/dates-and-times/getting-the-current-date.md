---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:35.591838-07:00
description: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong Python c\xF3 ngh\u0129a l\xE0\
  \ truy c\u1EADp ng\xE0y \u0111ang di\u1EC5n ra t\u1EEB h\u1EC7 th\u1ED1ng m\xE0\
  \ n\xF3 \u0111ang ch\u1EA1y. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1\
  u n\xE0y \u0111\u1EC3 ghi nh\u1EADt k\xFD, d\u1EA5u\u2026"
lastmod: '2024-03-11T00:14:09.348628-06:00'
model: gpt-4-0125-preview
summary: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong Python c\xF3 ngh\u0129a l\xE0\
  \ truy c\u1EADp ng\xE0y \u0111ang di\u1EC5n ra t\u1EEB h\u1EC7 th\u1ED1ng m\xE0\
  \ n\xF3 \u0111ang ch\u1EA1y. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1\
  u n\xE0y \u0111\u1EC3 ghi nh\u1EADt k\xFD, d\u1EA5u\u2026"
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Lấy ngày hiện tại trong Python có nghĩa là truy cập ngày đang diễn ra từ hệ thống mà nó đang chạy. Các lập trình viên làm điều này để ghi nhật ký, dấu thời gian, hoặc bất kỳ khi nào cần ngày hôm nay cho giao diện người dùng hoặc báo cáo.

## Làm thế nào:
Sử dụng mô-đun `datetime`. Đó là một cách đơn giản:

```Python
from datetime import datetime

# Lấy ngày hiện tại
current_date = datetime.now().date()

# In nó ra
print(current_date)
```

Kết quả mẫu có thể trông như sau:

```
2023-04-12
```

Lưu ý: Kết quả phụ thuộc vào ngày bạn chạy mã. Rõ ràng rồi.

## Sâu hơn
Mô-đun `datetime` không thay đổi đáng kể qua các phiên bản Python gần đây. Nó là một phần của thư viện tiêu chuẩn của Python – một bộ công cụ không rắc rối để xử lý ngày và giờ. Có lựa chọn khác không? Chắc chắn rồi, có `time`, nhưng nó thô sơ hơn. Đối với những công việc nặng nhọc, thế giới nhìn về `dateutil` và `arrow`, nhưng chỉ là để lấy ngày hôm nay thôi à? Hãy bám lấy `datetime`.

Bên dưới giao diện, `datetime.now()` chộp lấy thời điểm hiện tại theo cài đặt thời gian của máy tính bạn. Để nhận biết múi giờ, bạn sử dụng `datetime.now(timezone.utc)`, chẳng hạn. Theo lịch sử, đối phó với múi giờ luôn là một cơn đau đầu, vì vậy luôn cân nhắc vị trí và giờ linh hoạt nếu nó quan trọng.

Để có một ngày nhanh chóng không kèm theo dấu thời gian – như tạo một tệp với ngày hôm nay trong tên của nó – `datetime.now().date()` cung cấp cho bạn chính xác điều đó: một đối tượng ngày, bao gồm năm, tháng và ngày.

## Xem thêm
- Tài liệu chính thức Python về `datetime`: https://docs.python.org/3/library/datetime.html
- `arrow` để xử lý ngày/giờ phức tạp hơn: https://arrow.readthedocs.io
- `dateutil`, bởi vì múi giờ: https://dateutil.readthedocs.io
- Cài đặt thời gian của máy tính khiêm tốn của bạn bởi vì, chà, đó là nơi Python nhìn trước tiên.

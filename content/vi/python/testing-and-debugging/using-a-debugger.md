---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:57.511727-07:00
description: "H\xE3y c\xF9ng ph\xE2n t\xEDch c\xE1ch s\u1EED d\u1EE5ng `pdb`, debugger\
  \ c\xF3 s\u1EB5n c\u1EE7a Python. H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng m\u1ED9\
  t file c\xF3 t\xEAn l\xE0 `buggy.py`, ch\u1EE9a m\u1ED9t l\u1ED7i kh\xF3 t\xECm:\
  \ ```Python def\u2026"
lastmod: '2024-03-13T22:44:36.103016-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y c\xF9ng ph\xE2n t\xEDch c\xE1ch s\u1EED d\u1EE5ng `pdb`, debugger\
  \ c\xF3 s\u1EB5n c\u1EE7a Python."
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
weight: 35
---

## Làm thế nào:
Hãy cùng phân tích cách sử dụng `pdb`, debugger có sẵn của Python. Hãy tưởng tượng một file có tên là `buggy.py`, chứa một lỗi khó tìm:

```Python
def add_one(so):
    ket_qua = so ++ 1
    return ket_qua

print(add_one(7))
```

Khi bạn thực thi script này, bạn mong chờ `8`, nhưng nó chỉ báo lỗi cú pháp. Giờ là lúc để debugger ra tay!

Trên terminal, chạy:
```bash
python -m pdb buggy.py
```

Bạn sẽ bước vào debugger, và nó trông như thế này:
```Python
> /duong_dan_toi_file/buggy.py(1)<module>()
-> def add_one(so):
```

Sử dụng `l(ist)` để xem nhiều code hơn, `n(ext)` để đi tới dòng tiếp theo, hoặc `c(ontinue)` để tiếp tục chạy script. Khi bạn gặp lỗi, `pdb` sẽ dừng lại và cho bạn kiểm tra.

Sau khi bạn sửa `so ++ 1` thành `so + 1`, khởi động lại debugger để kiểm tra sửa chữa.
Nhớ là, bạn bè không để bạn bè code mà không có lưới. Đủ nói.

## Đào sâu
Trở lại Thời kì Tối tăm của lập trình (còn được biết đến là trước khi môi trường phát triển tích hợp, hay IDE, trở nên phổ biến), debugger thường là những công cụ độc lập mà bạn sử dụng bên ngoài trình soạn thảo văn bản. Chúng đến để cứu rỗi bằng cách cho phép lập trình viên kiểm tra trạng thái của phần mềm tại các điểm thực thi khác nhau.

Tính đến năm 2023, `pdb` của Python không phải là lựa chọn duy nhất. Mọi người có thể sử dụng IDE như PyCharm hay Visual Studio Code, mà có debugger riêng được tích hợp sẵn. Những công cụ này thêm vào các tính năng tiện lợi như breakpoints mà bạn có thể đặt bằng một cú nhấp chuột, thay vì phải gõ những lệnh bí ẩn.

Sau đó là `ipdb`, một gói có thể cài đặt qua pip mang đến vẻ đẹp của `IPython` cho quá trình debug. Đó là như `pdb` được tăng cường hiệu suất, với tự động hoàn thiện và làm nổi bật cú pháp.

Debugger cũng khác nhau về cách thức triển khai. Một số tiếp cận gần gũi với việc thực hiện chương trình ở cấp độ mã máy hoặc mã byte. Những cái khác, như nhiều debugger của ngôn ngữ cấp cao, thực thi mã trong một môi trường đặc biệt giám sát trạng thái biến và kiểm soát dòng chảy thực thi.

## Xem thêm
Để có cái nhìn đầy đủ về debugger của chính Python, hãy xem:
- Tài liệu `pdb`: https://docs.python.org/3/library/pdb.html

Nếu bạn tò mò về các phương án thay thế, những đường link này sẽ hữu ích:
- Kho lưu trữ và hướng dẫn sử dụng `ipdb`: https://github.com/gotcha/ipdb
- Debug với Visual Studio Code: https://code.visualstudio.com/docs/python/debugging
- Tính năng debug của PyCharm: https://www.jetbrains.com/help/pycharm/debugging-code.html

Chúc bạn may mắn trong việc săn lỗi!

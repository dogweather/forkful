---
title:                "Sử dụng mảng liên kết"
aliases:
- /vi/python/using-associative-arrays.md
date:                  2024-01-30T19:13:22.928821-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng mảng liên kết"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Mảng kết hợp, được biết đến trong Python là từ điển (dictionaries), ánh xạ khóa với giá trị, làm cho việc lấy, sửa đổi hoặc theo dõi dữ liệu bằng một định danh duy nhất trở nên dễ dàng. Lập trình viên sử dụng chúng vì hiệu quả trong việc truy cập các phần tử cũng như sự linh hoạt trong việc biểu diễn cấu trúc dữ liệu phức tạp.

## Làm thế nào:

Việc tạo một từ điển trong Python là trực tiếp. Bạn đặt các cặp khóa-giá trị trong dấu ngoặc nhọn `{}`, với khóa và giá trị được phân tách bởi dấu hai chấm:

```Python
# Tạo một mảng kết hợp (từ điển)
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

Kết quả:
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

Truy cập một giá trị bằng khóa của nó là đơn giản:

```Python
# Truy cập một giá trị
print(my_dict["name"])
```

Kết quả:
```
John
```

Thêm hoặc cập nhật các phần tử được thực hiện bằng cách gán một giá trị cho khóa:

```Python
# Thêm một cặp khóa-giá trị mới
my_dict["email"] = "john@example.com"
# Cập nhật một giá trị
my_dict["age"] = 31
print(my_dict)
```

Kết quả:
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

Để lặp qua các mục trong từ điển:

```Python
# Lặp qua các cặp khóa-giá trị
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

Kết quả:
```
name: John
age: 31
city: New York
email: john@example.com
```

## Sâu hơn

Mảng kết hợp trong Python, hay từ điển, được giới thiệu để cung cấp một cấu trúc dữ liệu cho việc truy cập và thao tác dữ liệu một cách hiệu quả. Không giống như các dãy số, mà được lập chỉ mục bởi một dải số, từ điển được lập chỉ mục bởi khóa, có thể là bất kỳ loại không thay đổi nào. Lựa chọn thiết kế này làm cho từ điển phù hợp lý tưởng cho các bảng tra cứu nhanh nơi khóa ánh xạ tới giá trị duy nhất.

Về mặt lịch sử, từ điển Python đã được thực thi sử dụng một bảng băm, đảm bảo rằng độ phức tạp thời gian trung bình cho các thao tác tra cứu, chèn và xóa là O(1). Trong Python 3.6 và sau đó, từ điển còn duy trì thứ tự chèn của các mục, kết hợp lợi ích của bảng băm với tính dự đoán của thứ tự chèn thấy trong các cấu trúc dữ liệu có thứ tự.

Mặc dù từ điển có tính đa năng vô cùng, trong một số trường hợp chuyên biệt, các lựa chọn thay thế như `collections.defaultdict` hoặc `collections.OrderedDict` (trước Python 3.7) có thể được ưa chuộng hơn. `defaultdict` đặc biệt hữu ích khi bạn cần một từ điển trả về một giá trị mặc định cho các khóa không tồn tại, làm đơn giản hóa một số loại logic điều kiện. Tuy nhiên, với sự cải thiện và phát triển liên tục của Python, lớp từ điển tích hợp thường vẫn là lựa chọn hàng đầu cho mảng kết hợp do sự ổn định và tiện lợi mà nó cung cấp ngay từ ban đầu.

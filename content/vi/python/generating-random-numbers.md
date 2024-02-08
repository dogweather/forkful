---
title:                "Sinh số ngẫu nhiên"
aliases:
- vi/python/generating-random-numbers.md
date:                  2024-01-28T22:01:43.362342-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sinh số ngẫu nhiên"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tạo ra số ngẫu nhiên bao gồm việc tạo ra các số không thể dự đoán một cách hợp lý hơn so với ngẫu nhiên, điều này rất cần thiết cho việc phát triển các mô phỏng, trò chơi và thuật toán bảo mật. Lập trình viên làm điều này để giới thiệu tính không chắc chắn hoặc mô phỏng các hiện tượng thực tế trong ứng dụng của họ.

## Cách thực hiện:

Python cung cấp mô-đun `random` giúp tạo ra các số ngẫu nhiên cho nhiều mục đích sử dụng. Dưới đây là cách bắt đầu:

1. **Nhập mô-đun**
    ```Python
    import random
    ```

2. **Tạo một Số Nguyên Ngẫu Nhiên**
    Giữa bất kỳ hai số nào.
    ```Python
    random_integer = random.randint(1, 10)
    print(random_integer)
    ```
    Kết quả mẫu: `7`

3. **Tạo một Số Thực**
    Giữa 0 và 1.
    ```Python
    random_float = random.random()
    print(random_float)
    ```
    Kết quả mẫu: `0.436432634653`
    
    Nếu bạn cần một số thực trong phạm vi khác, nhân lên:
    ```Python
    random_float_range = random.random() * 5  # từ 0 đến 5
    print(random_float_range)
    ```
    Kết quả mẫu: `3.182093745`

4. **Chọn một Phần Tử Ngẫu Nhiên từ Danh Sách**
    ```Python
    greetings = ['Hello', 'Hi', 'Hey', 'Hola', 'Bonjour']
    print(random.choice(greetings))
    ```
    Kết quả mẫu: `Hola`

5. **Xáo Trộn một Danh Sách**
    Hoàn hảo cho trò chơi bài hoặc bất kỳ ứng dụng nào cần ngẫu nhiên thứ tự.
    ```Python
    numbers = list(range(10))
    random.shuffle(numbers)
    print(numbers)
    ```
    Kết quả mẫu: `[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## Đào Sâu Hơn

Mô-đun `random` trong Python sử dụng một bộ tạo số giả ngẫu nhiên (PRNG), cụ thể là thuật toán Mersenne Twister, rất tốt cho các ứng dụng mục tiêu tổng quát nhưng không phù hợp cho mục đích mật mã do dự đoán được nếu đủ số lượng đầu ra được quan sát. Mô-đun `secrets`, được giới thiệu trong Python 3.6, cung cấp một lựa chọn tốt hơn cho việc tạo ra số ngẫu nhiên mạnh về mặt mật mã, đặc biệt hữu ích trong các ứng dụng nhạy cảm về bảo mật. Ví dụ, tạo một mã token ngẫu nhiên, bảo mật cho liên kết đặt lại mật khẩu:

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

Lịch sử, việc tạo ra các số ngẫu nhiên thực sự ngẫu nhiên đã là một thách thức trong lĩnh vực tính toán, với các phương pháp đầu tiên dựa vào hiện tượng vật lý hoặc các hạt giống nhập vào thủ công. Sự phát triển và áp dụng các thuật toán như Mersenne Twister (được sử dụng mặc định trong mô-đun `random` của Python ít nhất là cho đến lần cập nhật kiến thức cuối cùng của tôi vào năm 2023) đã đánh dấu sự tiến bộ đáng kể. Tuy nhiên, sự tìm kiếm liên tục cho các thuật toán an toàn và hiệu quả hơn đã dẫn đến việc bổ sung mô-đun `secrets` cho các nhiệm vụ liên quan đến mật mã. Sự tiến hóa này phản ánh tầm quan trọng ngày càng tăng của bảo mật trong phát triển phần mềm và nhu cầu về ngẫu nhiên mạnh mẽ hơn trong các ứng dụng từ mã hóa đến tạo token bảo mật.

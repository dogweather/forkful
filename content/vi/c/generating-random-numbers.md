---
title:                "Sinh số ngẫu nhiên"
date:                  2024-01-28T22:01:47.380531-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sinh số ngẫu nhiên"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tạo số ngẫu nhiên trong C liên quan đến việc tạo ra chuỗi số không có bất kỳ mô hình nhận dạng nào, mô phỏng khái niệm về sự ngẫu nhiên. Các lập trình viên sử dụng số ngẫu nhiên cho hàng loạt mục đích, bao gồm mô phỏng dữ liệu, ứng dụng mật mã học và phát triển trò chơi, khiến nó trở thành một khía cạnh quan trọng của lập trình.

## Làm thế nào:

Để tạo số ngẫu nhiên trong C, bạn thường sử dụng hàm `rand()` được tìm thấy trong `stdlib.h`. Tuy nhiên, điều quan trọng là phải cấy mầm cho trình sinh số ngẫu nhiên để đảm bảo tính biến thiên trong các số được tạo ra qua các lần thực thi chương trình khác nhau. Hàm `srand()`, đã cấy mầm với một giá trị, thường là thời gian hiện tại, hỗ trợ điều này.

Dưới đây là một ví dụ đơn giản về việc tạo một số ngẫu nhiên từ 0 đến 99:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Cấy mầm cho trình sinh số ngẫu nhiên
    srand((unsigned) time(NULL));

    // Tạo một số ngẫu nhiên từ 0 đến 99
    int randomNumber = rand() % 100;

    // In số ngẫu nhiên
    printf("Số Ngẫu Nhiên: %d\n", randomNumber);

    return 0;
}
```

Kết quả mẫu:

```
Số Ngẫu Nhiên: 42
```

Điều quan trọng cần lưu ý là mỗi lần thực thi chương trình này sẽ tạo ra một số ngẫu nhiên mới, nhờ vào việc cấy mầm với thời gian hiện tại.

## Sâu hơn

Cách truyền thống để tạo số ngẫu nhiên trong C, sử dụng `rand()` và `srand()`, không thực sự ngẫu nhiên. Nó là giả ngẫu nhiên. Điều này ổn cho nhiều ứng dụng, nhưng không đủ trong các tình huống cần độ ngẫu nhiên cao, ví dụ như trong một số ứng dụng mật mã học nghiêm túc. Chuỗi được tạo bởi `rand()` hoàn toàn được xác định bởi hạt giống cung cấp cho `srand()`. Do đó, nếu hạt giống được biết, chuỗi có thể được dự đoán, làm giảm sự ngẫu nhiên.

Trong quá khứ, hàm `rand()` đã bị chỉ trích vì sự ngẫu nhiên kém chất lượng và phạm vi hạn chế. Các phương pháp hiện đại bao gồm sử dụng các API đặc trưng cho thiết bị hoặc thư viện bên ngoài cung cấp gần đúng với sự ngẫu nhiên thực sự hơn hoặc, trong các hệ thống giống UNIX, đọc từ `/dev/random` hoặc `/dev/urandom` cho mục đích mật mã học.

Ví dụ, sử dụng `/dev/urandom` trong C:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    unsigned int randomNumber;

    // Mở /dev/urandom để đọc
    fp = fopen("/dev/urandom", "r");

    // Đọc một số ngẫu nhiên
    fread(&randomNumber, sizeof(randomNumber), 1, fp);

    // In số ngẫu nhiên
    printf("Số Ngẫu Nhiên: %u\n", randomNumber);

    // Đóng tập tin
    fclose(fp);

    return 0;
}
```

Phương pháp này đọc trực tiếp từ hồ năng lượng entropy của hệ thống, cung cấp chất lượng ngẫu nhiên cao hơn phù hợp cho các ứng dụng nhạy cảm hơn. Tuy nhiên, cách tiếp cận này có thể gặp phải vấn đề về tính di động trên các nền tảng khác nhau, làm cho nó ít phổ quát hơn so với sử dụng `rand()`.

Bất kể phương pháp nào, việc hiểu về bản chất của sự ngẫu nhiên và cách triển khai nó trong C là cực kỳ quan trọng cho việc phát triển những ứng dụng hiệu quả, an toàn và hấp dẫn.

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:00.824135-07:00
description: "Vi\u1EC7c t\u1EA1o ra c\xE1c s\u1ED1 ng\u1EABu nhi\xEAn trong C li\xEA\
  n quan \u0111\u1EBFn vi\u1EC7c t\u1EA1o ra c\xE1c gi\xE1 tr\u1ECB kh\xF4ng th\u1EC3\
  \ d\u1EF1 \u0111o\xE1n tr\u01B0\u1EDBc v\xE0 tu\xE2n theo m\u1ED9t ph\xE2n ph\u1ED1\
  i c\u1EE5 th\u1EC3, ch\u1EB3ng h\u1EA1n nh\u01B0 \u0111\u1ED3ng\u2026"
lastmod: '2024-03-13T22:44:37.262066-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o ra c\xE1c s\u1ED1 ng\u1EABu nhi\xEAn trong C li\xEAn\
  \ quan \u0111\u1EBFn vi\u1EC7c t\u1EA1o ra c\xE1c gi\xE1 tr\u1ECB kh\xF4ng th\u1EC3\
  \ d\u1EF1 \u0111o\xE1n tr\u01B0\u1EDBc v\xE0 tu\xE2n theo m\u1ED9t ph\xE2n ph\u1ED1\
  i c\u1EE5 th\u1EC3, ch\u1EB3ng h\u1EA1n nh\u01B0 \u0111\u1ED3ng \u0111\u1EC1u ho\u1EB7\
  c chu\u1EA9n."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Cái gì và Tại sao?

Việc tạo ra các số ngẫu nhiên trong C liên quan đến việc tạo ra các giá trị không thể dự đoán trước và tuân theo một phân phối cụ thể, chẳng hạn như đồng đều hoặc chuẩn. Khả năng này rất quan trọng đối với các ứng dụng từ mô phỏng và trò chơi tới các hoạt động mật mã, nơi tính không dự đoán trước hoặc việc mô phỏng sự ngẫu nhiên của thế giới thực là thiết yếu.

## Làm thế nào:

Trong C, số ngẫu nhiên có thể được tạo ra sử dụng hàm `rand()`, là một phần của thư viện tiêu chuẩn C `<stdlib.h>`. Theo mặc định, `rand()` tạo ra các số giả ngẫu nhiên trong phạm vi từ 0 đến `RAND_MAX` (hằng số được định nghĩa trong `<stdlib.h>`). Để kiểm soát chặt chẽ hơn về phạm vi, các lập trình viên có thể điều chỉnh kết quả đầu ra của `rand()`.

Dưới đây là một ví dụ đơn giản về việc tạo ra một số ngẫu nhiên từ 0 đến 99:

```c
#include <stdio.h>
#include <stdlib.h> // Cho rand() và srand()
#include <time.h>   // Cho time()

int main() {
    // Gieo hạt cho bộ sinh số ngẫu nhiên
    srand((unsigned) time(NULL));

    // Tạo ra một số ngẫu nhiên từ 0 đến 99
    int randomNumber = rand() % 100;

    printf("Số Ngẫu Nhiên: %d\n", randomNumber);

    return 0;
}
```

Kết quả mẫu có thể thay đổi mỗi lần bạn chạy chương trình này:

```
Số Ngẫu Nhiên: 42
```
Để tạo ra số ngẫu nhiên trong một phạm vi khác, bạn có thể điều chỉnh toán tử mô-đun (`%`) một cách phù hợp. Ví dụ, `rand() % 10` tạo ra các số từ 0 đến 9.

Quan trọng là phải ghi nhận rằng, việc gieo hạt cho bộ sinh số giả ngẫu nhiên (`srand()`) bằng thời gian hiện tại (`time(NULL)`) đảm bảo các chuỗi số ngẫu nhiên khác nhau qua các lần thực thi chương trình. Nếu không gieo hạt (`srand()`), `rand()` sẽ tạo ra cùng một chuỗi số mỗi lần chương trình được chạy.

## Sâu hơn nữa

Hàm `rand()` và hàm gieo hạt tương ứng `srand()` đã là một phần của thư viện tiêu chuẩn C trong nhiều thập kỷ. Chúng dựa trên các thuật toán tạo ra các chuỗi số chỉ có vẻ là ngẫu nhiên—do đó thuật ngữ "giả ngẫu nhiên." Thuật toán tiềm ẩn trong `rand()` thường là một bộ sinh số tuyến tính đồng nhất (LCG).

Mặc dù `rand()` và `srand()` đủ cho nhiều ứng dụng, chúng có những hạn chế được biết đến, đặc biệt liên quan đến chất lượng của sự ngẫu nhiên và khả năng dự đoán. Đối với các ứng dụng yêu cầu sự ngẫu nhiên cao cấp, chẳng hạn như các hoạt động mật mã, các lựa chọn thay thế như `/dev/random` hoặc `/dev/urandom` (trên các hệ thống giống Unix), hoặc các API do các thư viện mật mã cung cấp, nên được xem xét.

Với sự ra đời của C11, tiêu chuẩn ISO C đã bao gồm một header mới, `<stdatomic.h>`, cung cấp một sự kiểm soát tinh tế hơn cho các thao tác đồng thời, nhưng không trực tiếp liên quan đến sự ngẫu nhiên. Để đạt được sự ngẫu nhiên thực sự trong C, các nhà phát triển thường chuyển sang các thư viện cụ thể của nền tảng hoặc bên ngoài cung cấp các thuật toán tốt hơn hoặc tận dụng các nguồn entropy phần cứng.

Nhớ rằng, trong khi `rand()` phục vụ như một phương tiện đơn giản và dễ tiếp cận để tạo ra số giả ngẫu nhiên, việc sử dụng nó trong các ứng dụng hiện đại bị hạn chế bởi chất lượng và sự dự đoán của đầu ra. Khi cần các giải pháp mạnh mẽ hơn, đặc biệt là đối với các ứng dụng quan tâm đến bảo mật, việc tìm kiếm ngoài thư viện tiêu chuẩn được khuyến khích cao.

---
title:                "Sinh số ngẫu nhiên"
aliases:
- /vi/vba/generating-random-numbers.md
date:                  2024-02-01T21:54:44.695399-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sinh số ngẫu nhiên"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/vba/generating-random-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tạo ra các số ngẫu nhiên trong Visual Basic for Applications (VBA) cho phép các chương trình mô phỏng các quy trình với các yếu tố may rủi hoặc biến đổi, như lăn xúc xắc hay lấy mẫu dữ liệu. Các lập trình viên sử dụng những kỹ thuật này để phát triển mô hình, trò chơi hoặc các mô phỏng nơi mà các kết quả dễ đoán sẽ không thực tế hoặc ít hữu ích.

## Làm thế nào:

Trong VBA, hàm `Rnd` được sử dụng để tạo ra số ngẫu nhiên. Theo mặc định, `Rnd` tạo ra một số điểm dấu chấm nổi đơn chính xác lớn hơn hoặc bằng 0 và nhỏ hơn 1. Dưới đây là một số bước và ví dụ để khai thác hiệu quả số ngẫu nhiên:

1. **Số Ngẫu Nhiên Đơn Giản:**
   Để tạo một số ngẫu nhiên cơ bản, bạn chỉ cần gọi `Rnd()`:

   ```vb
   Sub GenerateRandomNumber()
       Dim randomNumber As Single
       randomNumber = Rnd() ' Số ngẫu nhiên giữa 0 và 1
       MsgBox randomNumber
   End Sub
   ```

2. **Thiết Lập Seed:**
   Câu lệnh `Randomize` khởi tạo bộ sinh số ngẫu nhiên, điều này có thể rất quan trọng để đảm bảo kết quả khác nhau mỗi khi code VBA của bạn chạy:

   ```vb
   Sub SeedRandomNumber()
       Randomize
       Dim randomNumber As Single
       randomNumber = Rnd()
       MsgBox randomNumber
   End Sub
   ```

3. **Tạo Số trong Phạm Vi:**
   Thông thường, bạn sẽ muốn một số ngẫu nhiên trong một phạm vi cụ thể. Dưới đây là cách tạo một số giữa 1 và 100:

   ```vb
   Sub RandomNumberInRange()
       Randomize
       Dim randomNumber As Integer
       randomNumber = Int((100 * Rnd()) + 1) ' Số ngẫu nhiên giữa 1 và 100
       MsgBox randomNumber
   End Sub
   ```

### Đầu Ra Mẫu:
Sau khi chạy `RandomNumberInRange`, bạn có thể thấy một hộp thoại hiển thị một số như `45`.

## Đi Sâu:

Hàm `Rnd` trong VBA, mặc dù dễ sử dụng, thực ra lại tạo ra các số giả ngẫu nhiên dựa trên một thuật toán định trước. Điều này có nghĩa là các chuỗi số nó tạo ra không thực sự ngẫu nhiên nhưng thường đủ cho nhiều nhiệm vụ cần quá trình stochastis.

Lịch sử, khả năng tạo số ngẫu nhiên trong VBA đã có từ những phiên bản đầu của Basic, thích nghi theo thời gian để bao gồm các tính năng như `Randomize` để cải thiện tính ngẫu nhiên bằng cách gieo hạt thuật toán với một điểm bắt đầu. Tuy nhiên, đối với các ứng dụng cần mức độ ngẫu nhiên cao như các hoạt động mật mã bảo mật, `Rnd` của VBA có thể không phải là công cụ tốt nhất. Các lựa chọn khác trong môi trường lập trình mạnh mẽ hơn hoặc ngôn ngữ được thiết kế với mật mã hóa trong tâm trí như mô-đun `secrets` của Python hoặc `SecureRandom` của Java nên được xem xét.

Mặc dù có những hạn chế, sự đơn giản và khả năng tiếp cận của việc tạo số ngẫu nhiên trong VBA tiếp tục làm cho nó trở thành một công cụ hữu ích cho một loạt các ứng dụng nhẹ, công việc mô phỏng và mục đích giáo dục.

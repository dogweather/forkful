---
title:                "Khởi đầu một dự án mới"
aliases: - /vi/vba/starting-a-new-project.md
date:                  2024-02-01T22:03:27.197787-07:00
model:                 gpt-4-0125-preview
simple_title:         "Khởi đầu một dự án mới"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/vba/starting-a-new-project.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?

Việc bắt đầu một dự án mới trong Visual Basic for Applications (VBA) bao gồm việc thiết lập một môi trường trong một ứng dụng chủ như Excel để tự động hóa các tác vụ hoặc mở rộng chức năng. Các lập trình viên khám phá lãnh thổ này để khai thác sức mạnh của VBA trong việc tùy chỉnh và tự động hóa các ứng dụng Microsoft Office, từ đó giúp luồng công việc trở nên mạch lạc hơn và tăng cường năng suất.

## Cách thực hiện:

Khi bạn sẵn lòng để bắt đầu một dự án VBA mới, điểm khởi đầu thường bao gồm việc truy cập vào trình soạn thảo VBA và khởi tạo khung dự án của bạn. Hãy cùng đi qua các bước sử dụng Excel là ứng dụng chủ:

1. **Mở Trình Soạn Thảo VBA**: Trong Excel, nhấn `Alt + F11` để truy cập trình soạn thảo VBA.
2. **Chèn Một Mô-đun Mới**: Từ menu, điều hướng đến `Chèn > Mô-đun` để thêm một mô-đun mới vào dự án của bạn. Đây là nơi chứa mã của bạn.
3. **Viết Macro Đầu Tiên**: Hãy viết một macro đơn giản hiển thị một hộp thông báo. Gõ mã sau vào mô-đun:

```vb
Sub SayHello()
    MsgBox "Xin chào, Thế giới!", vbInformation, "Lời chào"
End Sub
```

4. **Chạy Macro của Bạn**: Nhấn `F5` khi con trỏ của bạn đang ở trong sub `SayHello` hoặc đi đến `Chạy > Chạy Sub/UserForm` và chọn `SayHello`. Bạn sẽ thấy một hộp thông báo xuất hiện với "Xin chào, Thế giới!" và một nút "OK".

Kết quả Mẫu:

```plaintext
Một hộp thông báo với "Xin chào, Thế giới!" được hiển thị.
```

5. **Lưu Dự Án của Bạn**: Trước khi thoát, hãy đảm bảo bạn lưu công việc của mình. Nếu sổ làm việc Excel của bạn trước đó chưa được lưu, bạn sẽ được yêu cầu lưu dưới dạng sổ làm việc có kích hoạt macro (định dạng tệp `.xlsm`).

## Đi Sâu vào Đề Tài

Visual Basic for Applications đã là một trụ cột trong các chiến lược tự động hóa của Microsoft kể từ khi nó được giới thiệu vào năm 1993. Bắt nguồn là một sự tiến hóa của người tiền nhiệm, MacroBasic, VBA đã cung cấp một giải pháp mạnh mẽ hơn với tích hợp cải thiện trên toàn bộ bộ Office của Microsoft. Sự chuyển đổi sang VBA đã đánh dấu một bước ngoặt quan trọng, hướng tới những khả năng lập trình phức tạp hơn sử dụng sức mạnh của các ngôn ngữ lập trình đầy đủ.

Dù đã có tuổi đời, VBA vẫn đang phổ biến trong môi trường văn phòng hiện đại, phần lớn nhờ vào tích hợp sâu rộng trong các sản phẩm Office và lượng mã nguồn lưu trữ lớn tại nhiều tổ chức. Tuy nhiên, điều quan trọng cần lưu ý là, đối với các ứng dụng web mới hơn hoặc cho những tác vụ yêu cầu khả năng mở rộng và tích hợp với các ứng dụng ngoài Office, các ngôn ngữ và khung làm việc như Python với hệ sinh thái thư viện phong phú của nó, hoặc JavaScript cho Office Scripts, cung cấp một cách tiếp cận hiện đại và linh hoạt hơn. Những lựa chọn thay thế này, dù đòi hỏi đường cong học tập dốc và thiết lập ban đầu, cung cấp tính ứng dụng rộng lớn hơn và hỗ trợ cho các thực hành phát triển hiện đại như kiểm soát phiên bản và đường ống triển khai.

---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:29.843611-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c bao g\u1ED3m th\u1EF1c hi\u1EC7\
  n c\xE1c ph\xE9p to\xE1n to\xE1n h\u1ECDc tr\xEAn nh\u1EEFng s\u1ED1 c\xF3 c\u1EA3\
  \ ph\u1EA7n th\u1EF1c v\xE0 ph\u1EA7n \u1EA3o. C\xE1c l\u1EADp tr\xECnh vi\xEAn\
  \ th\u01B0\u1EDDng xuy\xEAn g\u1EB7p ph\u1EA3i s\u1ED1 ph\u1EE9c\u2026"
lastmod: '2024-03-11T00:14:09.686917-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c bao g\u1ED3m th\u1EF1c hi\u1EC7\
  n c\xE1c ph\xE9p to\xE1n to\xE1n h\u1ECDc tr\xEAn nh\u1EEFng s\u1ED1 c\xF3 c\u1EA3\
  \ ph\u1EA7n th\u1EF1c v\xE0 ph\u1EA7n \u1EA3o. C\xE1c l\u1EADp tr\xECnh vi\xEAn\
  \ th\u01B0\u1EDDng xuy\xEAn g\u1EB7p ph\u1EA3i s\u1ED1 ph\u1EE9c\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Làm việc với số phức bao gồm thực hiện các phép toán toán học trên những số có cả phần thực và phần ảo. Các lập trình viên thường xuyên gặp phải số phức trong các lĩnh vực như kỹ thuật, vật lý, và mọi nơi yêu cầu giải quyết các phương trình không thể thực hiện chỉ với số thực.

## Làm thế nào:

Trong Visual Basic for Applications (VBA), việc xử lý số phức có thể kém trực tiếp so với các ngôn ngữ hỗ trợ bản địa cho chúng. Tuy nhiên, bạn có thể quản lý các thao tác phức tạp bằng cách tạo các hàm hoặc sử dụng các hàm thư viện sẵn có. Hãy khám phá một ví dụ cơ bản về phép cộng, trừ, nhân và chia số phức:

```vb
' Hàm để cộng số phức
Function AddComplex(x As String, y As String) As String
    Dim real1 As Double, imag1 As Double
    Dim real2 As Double, imag2 As Double
    
    ' Trích xuất phần thực và ảo từ các số phức
    real1 = Val(Split(x, "+")(0))
    imag1 = Val(Split(x, "+")(1))
    real2 = Val(Split(y, "+")(0))
    imag2 = Val(Split(y, "+")(1))
    
    ' Thực hiện phép cộng
    AddComplex = (real1 + real2) & "+" & (imag1 + imag2) & "i"
End Function

' Ví dụ sử dụng
Sub ExampleUsage()
    Dim result As String
    result = AddComplex("3+2i", "1+7i")
    Debug.Print "Kết quả của Phép cộng: " & result  ' Kết quả: Kết quả của Phép cộng: 4+9i
End Sub
```

Khi mô tả phép cộng, các cách tiếp cận tương tự có thể được điều chỉnh cho phép trừ, nhân và chia. Đối với các thao tác phức tạp hơn ngoài tính toán cơ bản, có thể đáng giá để khám phá các thư viện bên ngoài hoặc tích hợp các giải pháp hỗ trợ thao tác số phức một cách tự nhiên hơn.

## Sâu hơn:

VBA không bao gồm hỗ trợ tích hợp sẵn cho số phức, một khía cạnh mà nó tụt hậu so với các ngôn ngữ như Python, có một lớp số phức (`complex`) hoặc C++ với Thư viện Tiêu chuẩn của nó (`std::complex`). Về mặt lịch sử, nhu cầu để thao tác trực tiếp số phức trong VBA là tương đối hiếm, vì nó thường được sử dụng cho tự động hóa, thao tác với các ứng dụng Office, và các tác vụ truyền thống không yêu cầu tính toán toán học phức tạp. Khi VBA được hình thành và phát triển, các trường hợp sử dụng của nó chủ yếu tập trung vào ứng dụng kinh doanh chứ không phải tính toán khoa học, có thể giải thích sự thiếu sót này.

Đối với các tác vụ cần thực hiện thao tác số phức một cách rộng rãi, lập trình viên có thể thấy việc sử dụng ngôn ngữ hướng tới toán học nhiều hơn là có lợi. Tuy nhiên, đối với những người cam kết hoặc bị giới hạn bởi việc sử dụng VBA, viết hàm tùy chỉnh (như được minh họa) hoặc tích hợp với phần mềm có khả năng này (như MATLAB hoặc chính Excel đến một mức độ nào đó) là các con đường tiến lên khả thi. Dù có hạn chế, các giải pháp sáng tạo và tích hợp bên ngoài có thể mở rộng khả năng sử dụng của VBA vào các lĩnh vực mà nó không được thiết kế ban đầu, bao gồm cả làm việc với số phức.

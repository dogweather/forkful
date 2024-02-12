---
title:                "Bắt đầu một dự án mới"
aliases:
- vi/bash/starting-a-new-project.md
date:                  2024-01-28T22:08:48.186954-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bắt đầu một dự án mới"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Bắt đầu một dự án mới thường có nghĩa là tạo ra một cấu trúc thư mục và các tệp ban đầu—giống như việc xây dựng nền móng cho một ngôi nhà. Lập trình viên làm điều đó để tổ chức suy nghĩ, tệp, và nhiệm vụ, biến sự hỗn loạn thành một danh sách công việc gọn gàng.

## Làm thế nào:

Hãy tạo một script đơn giản để khởi động một dự án mới.

```Bash
#!/bin/bash

# Script thiết lập dự án

TEN_DU_AN=$1
THU_MUC_GOC=$(pwd)

# Hàm tạo thư mục
tao_thu_muc() {
    mkdir -p $TEN_DU_AN/{bin,src,doc,test}
    echo "Thư mục đã được tạo."
}

# Hàm tạo tệp ban đầu
tao_tep() {
    touch $TEN_DU_AN/README.md
    touch $TEN_DU_AN/src/main.sh
    echo "#!/bin/bash" > $TEN_DU_AN/src/main.sh
    chmod +x $TEN_DU_AN/src/main.sh
    echo "Tệp ban đầu đã được tạo."
}

# Hàm khởi tạo một kho chứa git
khoi_tao_git() {
    cd $TEN_DU_AN
    git init
    cd $THU_MUC_GOC
    echo "Kho chứa Git đã được khởi tạo."
}

# Thực thi chính
if [ -z "$TEN_DU_AN" ]; then
    echo "Vui lòng chỉ định tên dự án."
else
    tao_thu_muc
    tao_tep
    khoi_tao_git
    echo "Dự án '$TEN_DU_AN' đã được tạo."
fi
```
Kết quả mẫu sau khi chạy `bash setup.sh myproject`:

```Bash
Thư mục đã được tạo.
Tệp ban đầu đã được tạo.
Kho chứa Git trống đã được khởi tạo tại /đường/dẫn/tới/myproject/.git/
Dự án 'myproject' đã được tạo.
```

## Sâu hơn

Trước khi có script, chúng ta thường tạo thư mục và tệp một cách thủ công mỗi khi cần—công việc buồn tẻ và dễ mắc lỗi. Tự động hóa bằng script giảm thiểu sai sót và tăng tốc độ thực hiện.

Các phương án thay thế bao gồm các công cụ như Yeoman, phát triển dự án trên nhiều ngôn ngữ, nhưng đó giống như việc sử dụng máy khoan điện khi bạn chỉ cần một cái đinh tán.

Script ở trên cố ý đơn giản. Nó tạo một thư mục dự án, các thư mục phụ cho việc tổ chức (như `src` cho mã nguồn), và các tệp cần thiết (như `README.md`). Hơn nữa, nó thiết lập một kho chứa Git để bạn có thể lưu các phiên bản công việc của mình. Bạn có thể chỉnh sửa và thêm vào nó cho nhu cầu của từng dự án.

## Xem thêm

- Tài liệu Git: https://git-scm.com/doc
- Yeoman: http://yeoman.io/
- Hướng dẫn về script Bash: https://www.shellscript.sh/

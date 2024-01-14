---
title:    "Gleam: 임시 파일 만들기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜 Temporary 파일을 생성하는가?

Temporary 파일은 프로그래밍 중에 임시적으로 사용되는 파일입니다. 이러한 파일은 일시적인 데이터를 저장하고 처리하는 데 유용합니다.

## 어떻게 만드는가?

```Gleam
import gleam/temporary

let temp_file = temporary.create()
|> temporary.file

if temp_file.ok? do
  let file_path = temp_file.path
  let file_content = "This is a temporary file."
  temporary.write(file_path, file_content)
  |> temporary.delete
end
```

위의 코드는 `gleam/temporary` 라이브러리를 사용하여 Temporary 파일을 생성하는 방법을 나타냅니다. `temporary.create()` 함수를 사용하여 Temporary 파일을 생성하고, `temporary.file` 함수를 사용하여 파일 객체를 가져옵니다. `ok?` 함수를 사용하여 파일이 정상적으로 생성되었는지 확인하고, 생성된 파일 객체에서 `path` 속성을 사용하여 파일 경로를 가져옵니다. 그리고 Temporary 파일의 내용을 파일 경로에 해당하는 파일에 쓰고, 처리가 끝난 후 `temporary.delete` 함수를 사용하여 파일을 삭제합니다.

## 깊게 들어가보기

보다 자세한 내용은 `gleam/temporary` 라이브러리의 GitHub 페이지에서 확인할 수 있습니다. Temporary 파일의 생성과 사용 방법에 대한 더 많은 정보를 제공하고 있습니다.

# 참고자료

- `gleam/temporary` 라이브러리: https://github.com/gleam-lang/gleam_temporary
---
title:    "Ruby: 텍스트 파일 쓰기"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

Ruby에서 텍스트 파일을 작성하는 활동을 즐기는 이유에 대해 알아보겠습니다.

## 왜

텍스트 파일은 깨끗하고 정리된 형식으로 데이터를 저장할 수 있기 때문에, 프로그래밍에서 매우 유용합니다. 또한 코드 관리를 위해 자주 사용되며, 긴 코드 라인을 여러 개로 나누어 작업을 편리하게 만들어 줍니다.

## 방법

텍스트 파일을 작성하는 가장 간단한 방법은 Ruby의 `File` 클래스를 사용하는 것입니다. 아래 코드를 참고하세요.

```Ruby
# 새로운 파일 만들기
file = File.new("new_file.txt", "w")

# 파일에 쓰기
file.puts("Hello, world!")
file.close

# 기존 파일 열기
file = File.open("existing_file.txt", "a")

# 파일에 쓰기
file.puts("Hello, Ruby!")
file.close
```

위 코드에서 `File.new`를 사용하여 새로운 파일을 만들고, `File.open`으로 기존 파일을 열 수 있습니다. `"w"`나 `"a"`와 같은 파라미터는 파일을 쓰기 또는 추가하기 위해 열었는지를 나타냅니다. `puts` 메서드를 사용하여 파일에 내용을 쓸 수 있습니다. 이를 통해 텍스트 파일을 생성하고 내용을 쉽게 작성할 수 있습니다. 또한 `clo
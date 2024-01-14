---
title:                "Gleam: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜?

일시적인 파일을 생성하는 것은 개발자들에게 매우 중요한 작업입니다. 임시 파일은 프로그램 실행 중에 일시적으로 생성된 데이터를 저장하는 데 사용될 수 있습니다.

# 어떻게?

파일을 생성하려면 `tempfile` 모듈을 사용해야 합니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다.

```Gleam
import tempfile

{_, path} = tempfile.new()
```

위의 코드는 임시 파일을 생성하고, 해당 파일의 경로를 `path` 변수에 할당합니다. 임시 파일을 사용한 후에는 `delete` 함수를 호출하여 삭제해야 합니다.

```Gleam
import tempfile

{_, path} = tempfile.new()
// 임시 파일 사용
tempfile.delete(path)
```

# 깊게 파보기

파일의 생성과 삭제는 파일 시스템에 많은 부하를 줄 수 있습니다. 따라서, 임시 파일을 사용할 때에는 성능 문제를 고려해야 합니다. 매번 파일을 생성할 때마다 많은 시간이 소요된다면, 메모리에 임시 데이터를 저장하는 방법을 고려해볼 수 있습니다.

# 이어보기

다음은 임시 파일 생성에 관련된 링크들입니다.

- [Gleam 공식 문서 - tempfile 모듈](https://gleam.run/standard-library/#documentation_tempfile)
- [Python에서 임시 파일 생성하기](https://realpython.com/python-tempfile/#using-python-tempfile-module)
- [Linux에서 임시 파일 생성하기](https://www.geeksforgeeks.org/create-temporary-file-name-temp-file-function/)

# 참고

- [Markdown 가이드](https://www.markdownguide.org/basic-syntax/)
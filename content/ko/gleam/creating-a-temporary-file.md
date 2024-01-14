---
title:    "Gleam: 임시 파일 만들기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## 왜 Temporary File을 만들까요?

Temporary file은 프로그래밍에서 매우 중요한 역할을 합니다. 우리는 이 글에서 Temporary file이 왜 필요한지, 그리고 어떻게 만들 수 있는지에 대해 알아보겠습니다.

## 어떻게 만들까요?

Temporary file을 만드는 방법은 매우 간단합니다. 아래의 코드를 참고해주세요.

```Gleam
import gleam/file

file_temp := file.temp()
file_temp_path := file_temp.path()
```

위의 코드는 Gleam에서 file 모듈을 로드하고, `file.temp()` 함수를 사용하여 Temporary file 객체를 생성한 후, `file_temp.path()`를 호출해 해당 file의 경로를 얻는 과정을 보여줍니다. 아래는 위 코드의 예제 출력 예시입니다.

```
/tmp/gleamfileXXXXX
```

위와 같이 임의의 경로가 출력되는 것을 볼 수 있습니다. 이 경로는 임시적으로 생성된 파일이 저장되는 위치입니다.

## Deep Dive

Temporary file은 주로 프로그램이 작동하는 동안 임시적으로 사용되는 데이터를 저장하기 위해 사용됩니다. 이 파일들은 보통 프로그램이 종료되면 자동적으로 삭제되지만, 우리가 수동으로 삭제할 수도 있습니다. Temporary file은 프로그램간에 데이터를 교환할 때도 유용하게 사용될 수 있습니다.

Temporary file은 사용하기 간단한 대신, 보안에 취약할 수 있습니다. 따라서, 프로그래머는 만약 민감한 데이터를 저장해야 한다면, 다른 방법을 고려해야 할 것입니다.

## See Also

- <https://ohmydis.github.io/2017/10/25/create-tempfile-in-python/>
- <https://docs.python.org/ko/3/library/tempfile.html>
- <https://www.geeksforgeeks.org/how-to-create-a-temporary-file-in-python/>
---
title:    "Python: 임시 파일 생성하기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

"## 왜 만드는가"

프로그래밍을 배우고 나면 임시 파일을 생성해야 할 일이 있습니다. 임시 파일은 데이터를 저장하거나 코드를 실행할 때 다양한 용도로 사용되는데, 이를 통해 코드 실행 중에 발생할 수 있는 문제를 방지할 수 있습니다.

"## 생성하는 방법"

```Python
# 임시 파일 생성
import tempfile
temp_file = tempfile.NamedTemporaryFile()
```

위의 코드를 실행하면 운영 체제에 자동으로 임시 파일이 생성됩니다. 이 파일은 사용이 끝나면 자동으로 삭제되므로 따로 정리할 필요가 없습니다.

```Python
# 데이터 쓰기
temp_file.write("Hello World!")
# 데이터 읽기
temp_file.seek(0)
temp_file.read() # 결과: "Hello World!"
# 파일 닫기
temp_file.close() # 파일이 삭제됩니다.
```

위의 코드 예시처럼 데이터를 쓰고 읽은 뒤 파일을 닫으면 자동으로 삭제되는 것을 볼 수 있습니다. 임시 파일은 이처럼 간단하게 사용할 수 있습니다.

"## 깊이 알아보기"

프로그래밍 언어마다 임시 파일을 생성하는 방식은 다르지만, 대부분의 언어에서는 운영 체제에 임시 파일을 요청하는 방식으로 구현됩니다. 이를 통해 운영 체제의 시스템 자원을 활용할 수 있으며, 임시 파일을 생성할 때 사용자가 지정한 임시 파일의 경로나 파일명이 이미 존재하지 않는지도 자동으로 확인합니다. 이처럼 시스템 자원을 효율적으로 활용하고 사용자 편의성을 보장하기 위해 프로그래밍 언어에서 임시 파일을 생성하는 방식을 정교하게 구현합니다.

"## 더 알아보기"

- 임시 파일 관련 파이썬 문서: [https://docs.python.org/3/library/tempfile.html](https://docs.python.org/3/library/tempfile.html)
- 임시 파일 생성 예시: [https://www.geeksforgeeks.org/tempfile-module-in-python/](https://www.geeksforgeeks.org/tempfile-module-in-python/)
- 임시 파일 생성 방식 비교: [https://brunorocha.org/python/why-using-temp-file.html](https://brunorocha.org/python/why-using-temp-file.html)

"# 참고 자료"

- [Markdown 가이드](https://commonmark.org/help/)
- [Python 문서화 관련 가이드](https://www.python.org/dev/peps/pep-0257/)
- [Korean](https://en.wikipedia.org/wiki/Korean_language)
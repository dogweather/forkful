---
date: 2024-01-20 17:41:16.053021-07:00
description: "Temporary file, yaani asthayi file, ek aisi file hai jo aapko data ke\
  \ short-term storage ke liye chahiye hoti hai. Programmers ise test data, caching,\
  \ ya\u2026"
lastmod: '2024-03-11T00:14:25.463223-06:00'
model: gpt-4-1106-preview
summary: "Temporary file, yaani asthayi file, ek aisi file hai jo aapko data ke short-term\
  \ storage ke liye chahiye hoti hai. Programmers ise test data, caching, ya\u2026"
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

Temporary file, yaani asthayi file, ek aisi file hai jo aapko data ke short-term storage ke liye chahiye hoti hai. Programmers ise test data, caching, ya complex computations ke dauran intermediary results ko temporarily save karne ke liye use karte hain.

## How to: (कैसे करें:)

Python mein temporary file create karna bahut aasan hai. `tempfile` module ka use karke, aaiye dekhte hain:

```python
import tempfile

# Temporary file create karne ke liye:
with tempfile.TemporaryFile(mode='w+t') as tf:
    # Kuch data write karein:
    tf.write('Hello, yeh ek temporary line hai!')
    
    # Seek to the start of file (file ke shuruat mein jaaiye)
    tf.seek(0)
    
    # Data read karein:
    print(tf.read())  # Output: Hello, yeh ek temporary line hai!

# File automatically delete ho jaayegi with block ke bahar jaane par.
```

Yeh code ek temporary file banaega, usme kuch data write karega, phir data ko read karega, aur finally, file automatically delete ho jaayegi jaise hi `with` block khatm hoga.

## Deep Dive (गहराई में जानकारी):

Pehele ke zamaane mein, developers manually files create karte the aur use manage karte the, jo errors ka karan ban sakta tha. `tempfile` module Python mein aaya taaki secure aur asaan temporary file handling provide ki ja sake.

Alternatives mein `mkstemp()` aur `NamedTemporaryFile()` shamil hain, jo apko direct control dete hain, par safety aur suvidha ke maamle mein `TemporaryFile()` behtar hai.

Implementation ke baare mein, `tempfile` module OS-dependent functions ka istemaal karta hai taaki security aur performance ko ensure kiya ja sake. Python 'garbage collection' aur context managers jaise features ke saath work karta hai taki resources ka automatic management ho.

## See Also (और भी जानिए):

Yadi aap aur janna chahte hain to Python ki official documentation aur neeche diye hue resources ko dekhein:

- Python official tempfile documentation: https://docs.python.org/3/library/tempfile.html
- Context managers aur `with` statement: https://docs.python.org/3/reference/datamodel.html#context-managers
- File handling in Python: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files

Ye resources aapko Python mein temporary file handling ke baare mein gehrai se samjhaenge aur aapki programming skills ko aur bhi behtar banaenge.

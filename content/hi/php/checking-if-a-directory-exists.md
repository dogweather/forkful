---
title:    "PHP: डायरेक्टरी मौजूद है कि नहीं जाँच"
keywords: ["PHP"]
---

{{< edit_this_page >}}

"Kyu: Directory ki upasthiti jaanch karna kyu zaruri hai?"

"Directory ki upasthiti ko jaanch karna keval tab zaruri hai jab hume apne computer system mein koi naya file ya folder banana hai. Agar hum ek existent directory mein naya file ya folder banaayenge, toh hume pata hona chahiye ki woh directory upasthit hai ya nahi, nahi toh humari file ya folder bhi create nahi hogi."

"Kaise: Directory ki upasthiti jaanch karne ka tarika"

"```PHP
<?php
$directory = 'foldername/';
// Yeh function directory ki upasthiti ko check karega
if (is_dir($directory)) {
    echo 'Directory upasthit hai'; // Output: Directory upasthit hai
} else {
    echo 'Yeh directory upasthit nahi hai';
}
?>
```"

"Deep Dive: Directory ki upasthiti ko check karne ki puri jaankari"

"Directory ko check karne ke liye, hum is_dir() function ka istemaal karte hain. Yeh function hume boolean value (true or false) return karta hai. Agar directory upasthit hai toh true return hoga, aur agar directory upasthit nahi hai toh false return hoga. Iske alawa, hume is_link() function ka istemaal karke symlink (symbolic link) ko check kar sakte hain."

"See Also: Inhe bhi zaroor padhe"

- [PHP manual: is_dir() function](https://www.php.net/manual/en/function.is-dir.php)
- [PHP manual: is_link() function](https://www.php.net/manual/en/function.is-link.php)
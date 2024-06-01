#!/opt/homebrew/bin python3
import panflute as pf

def replace_unicode(elem, doc):
    if isinstance(elem, pf.Str):
        text = elem.text
        text = text.replace('\uFE0E', '')  # Replace the character U+FE0E
        elem.text = text

def main(doc=None):
    return pf.run_filter(replace_unicode, doc=doc)

if __name__ == '__main__':
    main()

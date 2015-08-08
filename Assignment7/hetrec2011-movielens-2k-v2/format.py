import codecs
import unicodedata


def remove_accents(input_str):
    nkfd_form = unicodedata.normalize('NFKD', input_str)
    only_ascii = nkfd_form.encode('ASCII', 'ignore')
    return only_ascii

programacao = codecs.open('/home/rodolfo/Projetos/DataAnalysis/Assignment7/hetrec2011-movielens-2k-v2/movies.dat',encoding='latin1')
f = open('/home/rodolfo/Projetos/DataAnalysis/Assignment7/hetrec2011-movielens-2k-v2/movies_m.dat', 'w')



for lines in programacao:
	f.write(remove_accents(lines.strip())+"\n")
	print remove_accents(lines.strip())



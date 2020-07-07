#'
#' The following methods could be implemented in the future.
#'


# update_user_information: https://developers.mercadolibre.com.co/es_ar/producto-consulta-usuarios#Actualizar-datos-de-usuario
    # I started trying to implement this in set_user_information in users.R
# require_mercadopago: https://developers.mercadolibre.com.co/es_ar/producto-consulta-usuarios#Mercado-Pago
# unrequire_mercadopago: https://developers.mercadolibre.com.co/es_ar/producto-consulta-usuarios#Mercado-Pago
# According to the API, there's a function /site_domains/{Site_domain_url}	Devuelve información sobre el dominio.	GET.
    # However, I don't know what site_domains are (didn't work with "MLA") and the example for this function refers to a
    # different function, so I'm going to skip it for now.
    # Link here: https://developers.mercadolibre.com.co/es_ar/categorias-y-publicaciones
# More useful / specific methods that return parts of the information produces by calling get_category_information.
# Some functions that use ids relating to higher-level ids (e.g. state_ids for countries with country_ids) require the country_id
  # for me to do error-checking even though they're not strictly required. Consider making these arguments optional or removing them entirely.
